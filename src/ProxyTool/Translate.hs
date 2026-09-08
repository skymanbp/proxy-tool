{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Request/response rewriting and cross-dialect translation.
--
-- Everything in this module is pure, so the interesting behaviour is testable
-- without a network or a subprocess.  "ProxyTool.Proxy" supplies the IO.
--
-- Two jobs live here:
--
--  * /Rewriting/ -- same dialect in and out.  Force the model, inject effort,
--    merge the provider's extra body.
--
--  * /Translation/ -- Anthropic Messages in, OpenAI Chat Completions out (and
--    the responses back again, including the streaming case).  This is what
--    lets an Anthropic-speaking agent drive an OpenAI-compatible endpoint.
--
-- Schema provenance: the Anthropic SSE event vocabulary
-- (@message_start@, @content_block_delta@, @input_json_delta@, ...) was
-- checked against the strings in the shipped @claude.exe@; the OpenAI chunk
-- and parameter shapes against openai-python 2.30.0
-- (@types\/chat\/chat_completion_chunk.py@, @completion_create_params.py@).
module ProxyTool.Translate
  ( -- * Options
    TranslateOpts (..)
  , defaultTranslateOpts
    -- * Same-dialect rewriting
  , rewriteAnthropicRequest
  , rewriteOpenAIRequest
    -- * Anthropic -> OpenAI
  , anthropicToOpenAIRequest
    -- * OpenAI -> Anthropic
  , openAIToAnthropicResponse
  , openAIErrorToAnthropic
    -- * Streaming
  , SseEvent (..)
  , renderSseEvent
  , splitSseLines
  , StreamState
  , newStreamState
  , streamSawAnything
  , stepStream
  , finishStream
    -- * Exposed for tests
  , decodeToolArguments
  , mapFinishReason
  , resolveModel
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import ProxyTool.Types

-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

-- | What the proxy should impose on requests passing through it.
data TranslateOpts = TranslateOpts
  { optModel     :: Maybe Text
    -- ^ Force this model, whatever the agent asked for.
  , optFastModel :: Maybe Text
    -- ^ Substitute for the agent's small/fast tier.  See 'resolveModel' for
    -- how a request is recognised as belonging to that tier.
  , optEffort    :: Maybe Effort
  , optExtraBody :: Maybe Object
    -- ^ Merged into the outgoing body last, so it wins over everything else.
  } deriving (Eq, Show)

defaultTranslateOpts :: TranslateOpts
defaultTranslateOpts = TranslateOpts Nothing Nothing Nothing Nothing

-- ---------------------------------------------------------------------------
-- Small JSON helpers
-- ---------------------------------------------------------------------------

asObject :: Value -> Maybe Object
asObject (Object o) = Just o
asObject _          = Nothing

asText :: Value -> Maybe Text
asText (String s) = Just s
asText _          = Nothing

asArray :: Value -> Maybe (V.Vector Value)
asArray (Array a) = Just a
asArray _         = Nothing

asInt :: Value -> Maybe Int
asInt (Number n) = toBoundedInteger n
asInt _          = Nothing

lookupText :: Key -> Object -> Maybe Text
lookupText k o = KM.lookup k o >>= asText

lookupObject :: Key -> Object -> Maybe Object
lookupObject k o = KM.lookup k o >>= asObject

lookupArray :: Key -> Object -> Maybe (V.Vector Value)
lookupArray k o = KM.lookup k o >>= asArray

lookupInt :: Key -> Object -> Maybe Int
lookupInt k o = KM.lookup k o >>= asInt

-- | @copyKey k src dst@ copies key @k@ across when present and not null.
copyKey :: Key -> Object -> Object -> Object
copyKey k src dst = case KM.lookup k src of
  Just Null -> dst
  Just v    -> KM.insert k v dst
  Nothing   -> dst

-- | Right-biased merge: entries of the second object win.
mergeObject :: Object -> Object -> Object
mergeObject base override = KM.union override base

-- ---------------------------------------------------------------------------
-- Model selection
-- ---------------------------------------------------------------------------

-- | Decide which model name to send upstream.
--
-- Agents that use a cheap second tier (Claude Code routes background work to
-- a Haiku-class model) do not announce that fact in the request; the only
-- signal on the wire is the model name itself.  So the fast tier is
-- recognised by name.  This is a heuristic, and it is deliberately narrow:
-- it fires only when a fast model is actually configured.
resolveModel :: TranslateOpts -> Maybe Text -> Maybe Text
resolveModel opts requested
  | Just fast <- optFastModel opts
  , Just req  <- requested
  , isFastTier req
  = Just fast
  | otherwise
  = optModel opts
  where
    isFastTier m =
      let lm = T.toLower m
      in any (`T.isInfixOf` lm) ["haiku", "mini", "flash", "small", "fast"]

-- ---------------------------------------------------------------------------
-- Same-dialect rewriting
-- ---------------------------------------------------------------------------

-- | Rewrite an Anthropic request that is going to an Anthropic endpoint.
--
-- Effort becomes extended thinking.  Two API constraints are enforced here
-- rather than left to blow up upstream: @budget_tokens@ must be at least
-- 1024 and strictly below @max_tokens@, and sampling parameters may not be
-- combined with thinking.
rewriteAnthropicRequest :: TranslateOpts -> Value -> Value
rewriteAnthropicRequest opts (Object o) = Object (applyExtra opts (withThinking withModel))
  where
    withModel = case resolveModel opts (lookupText "model" o) of
      Just m  -> KM.insert "model" (String m) o
      Nothing -> o

    withThinking obj = case optEffort opts >>= effortThinkingBudget of
      Nothing     -> obj
      Just wanted ->
        let requested = fromMaybe 4096 (lookupInt "max_tokens" obj)
            -- Give thinking room without shrinking what the agent asked for.
            maxTok    = max requested (wanted + 1024)
            budget    = min wanted (maxTok - 1)
        in if budget < 1024
             then obj
             else
               -- Anthropic rejects temperature/top_p/top_k alongside thinking.
               KM.delete "temperature"
             . KM.delete "top_p"
             . KM.delete "top_k"
             . KM.insert "max_tokens" (toJSON maxTok)
             . KM.insert "thinking" (object [ "type" .= ("enabled" :: Text)
                                            , "budget_tokens" .= budget ])
             $ obj
rewriteAnthropicRequest _ v = v

-- | Rewrite an OpenAI request that is going to an OpenAI endpoint.
rewriteOpenAIRequest :: TranslateOpts -> Value -> Value
rewriteOpenAIRequest opts (Object o) = Object (applyExtra opts (withEffort withModel))
  where
    withModel = case resolveModel opts (lookupText "model" o) of
      Just m  -> KM.insert "model" (String m) o
      Nothing -> o
    withEffort obj = case optEffort opts of
      Nothing -> obj
      Just e  -> KM.insert "reasoning_effort" (String (effortOpenAIWire e)) obj
rewriteOpenAIRequest _ v = v

applyExtra :: TranslateOpts -> Object -> Object
applyExtra opts obj = maybe obj (mergeObject obj) (optExtraBody opts)

-- ---------------------------------------------------------------------------
-- Anthropic request -> OpenAI request
-- ---------------------------------------------------------------------------

-- | Translate an Anthropic @\/v1\/messages@ body into an OpenAI
-- @\/v1\/chat\/completions@ body.
anthropicToOpenAIRequest :: TranslateOpts -> Value -> Either Text Value
anthropicToOpenAIRequest opts val = do
  o <- maybe (Left "request body is not a JSON object") Right (asObject val)
  msgs <- maybe (Left "request has no \"messages\" array") Right
            (lookupArray "messages" o)
  let systemMsgs = systemToOpenAI (KM.lookup "system" o)
      converted  = concatMap anthropicMessageToOpenAI (V.toList msgs)
      model      = fromMaybe (fromMaybe "" (lookupText "model" o))
                             (resolveModel opts (lookupText "model" o))
      base = KM.fromList $
        [ ("model",    String model)
        , ("messages", toJSON (systemMsgs <> converted))
        ]
        <> [ ("max_tokens", toJSON n) | Just n <- [lookupInt "max_tokens" o] ]
        <> [ ("stop", toJSON stops)
           | Just stops <- [lookupArray "stop_sequences" o]
           , not (V.null stops) ]
        <> [ ("tools", toJSON ts)
           | Just ts <- [toolsToOpenAI <$> lookupArray "tools" o]
           , not (null ts) ]
        <> [ ("tool_choice", tc)
           | Just tc <- [KM.lookup "tool_choice" o >>= toolChoiceToOpenAI] ]
      withSampling = copyKey "temperature" o
                   . copyKey "top_p" o
                   . copyKey "stream" o
                   $ base
      withUsage = case KM.lookup "stream" o of
        -- Ask for the trailing usage chunk so message_delta can report real
        -- output token counts instead of zero.
        Just (Bool True) ->
          KM.insert "stream_options" (object ["include_usage" .= True]) withSampling
        _ -> withSampling
      withEffort = case optEffort opts of
        Nothing -> withUsage
        Just e  -> KM.insert "reasoning_effort" (String (effortOpenAIWire e)) withUsage
  pure (Object (applyExtra opts withEffort))

-- | Anthropic carries the system prompt out of band; OpenAI wants it as the
-- first message.
systemToOpenAI :: Maybe Value -> [Value]
systemToOpenAI = \case
  Nothing         -> []
  Just Null       -> []
  Just (String s) | T.null s -> []
                  | otherwise -> [sys s]
  Just (Array a)  ->
    let txt = T.intercalate "\n\n" (mapMaybe textOfBlock (V.toList a))
    in [sys txt | not (T.null txt)]
  Just _          -> []
  where
    sys s = object ["role" .= ("system" :: Text), "content" .= s]

textOfBlock :: Value -> Maybe Text
textOfBlock v = do
  o <- asObject v
  case lookupText "type" o of
    Just "text" -> lookupText "text" o
    _           -> Nothing

-- | One Anthropic message can become several OpenAI messages: tool results
-- are their own @role: "tool"@ entries, and must not stay bundled inside the
-- user turn that carried them.
anthropicMessageToOpenAI :: Value -> [Value]
anthropicMessageToOpenAI v = fromMaybe [] $ do
  o <- asObject v
  let role = fromMaybe "user" (lookupText "role" o)
  pure $ case KM.lookup "content" o of
    Just (String s) -> [object ["role" .= role, "content" .= s]]
    Just (Array a)  -> blocksToOpenAI role (V.toList a)
    _               -> []

blocksToOpenAI :: Text -> [Value] -> [Value]
blocksToOpenAI role blocks
  | role == "assistant" = assistantSide
  | otherwise           = toolMessages <> userSide
  where
    objs = mapMaybe asObject blocks
    kindOf o = fromMaybe "" (lookupText "type" o)

    -- Assistant turn: text becomes content, tool_use becomes tool_calls.
    -- Anthropic thinking blocks are dropped: they are meaningless to an
    -- OpenAI endpoint and their signatures cannot be reconstructed.
    assistantSide =
      let texts = [t | o <- objs, kindOf o == "text", Just t <- [lookupText "text" o]]
          calls = [ toolUseToCall o | o <- objs, kindOf o == "tool_use" ]
          content = T.intercalate "" texts
          bodyBase = [("role", String "assistant")]
          bodyText = [("content", String content) | not (T.null content) || null calls]
          bodyCall = [("tool_calls", toJSON calls) | not (null calls)]
      in [Object (KM.fromList (bodyBase <> bodyText <> bodyCall))]

    toolUseToCall o = object
      [ "id"   .= fromMaybe "" (lookupText "id" o)
      , "type" .= ("function" :: Text)
      , "function" .= object
          [ "name" .= fromMaybe "" (lookupText "name" o)
          , "arguments" .= encodeArguments (fromMaybe (object []) (KM.lookup "input" o))
          ]
      ]

    -- User turn: every tool_result becomes its own tool message, emitted
    -- first so it directly follows the assistant turn that requested it.
    toolMessages =
      [ object [ "role" .= ("tool" :: Text)
               , "tool_call_id" .= fromMaybe "" (lookupText "tool_use_id" o)
               , "content" .= toolResultText o
               ]
      | o <- objs, kindOf o == "tool_result" ]

    userSide =
      let parts = mapMaybe userPart objs
      in [ object ["role" .= role, "content" .= parts] | not (null parts) ]

    userPart o = case kindOf o of
      "text"  -> (\t -> object ["type" .= ("text" :: Text), "text" .= t])
                   <$> lookupText "text" o
      "image" -> imagePart o
      _       -> Nothing

-- | Anthropic image blocks carry the bytes inline; OpenAI wants a URL, so
-- base64 sources are re-expressed as data URLs.
imagePart :: Object -> Maybe Value
imagePart o = do
  src <- lookupObject "source" o
  let url = case lookupText "type" src of
        Just "url"    -> lookupText "url" src
        Just "base64" -> do
          mt <- lookupText "media_type" src
          d  <- lookupText "data" src
          pure ("data:" <> mt <> ";base64," <> d)
        _ -> Nothing
  u <- url
  pure $ object
    [ "type" .= ("image_url" :: Text)
    , "image_url" .= object ["url" .= u]
    ]

-- | A tool result's content may be a string or a block list; OpenAI takes
-- text, so blocks are flattened.
toolResultText :: Object -> Text
toolResultText o = case KM.lookup "content" o of
  Just (String s) -> s
  Just (Array a)  -> T.intercalate "\n" (mapMaybe textOfBlock (V.toList a))
  Just v          -> TE.decodeUtf8 (BL.toStrict (encode v))
  Nothing         -> ""

encodeArguments :: Value -> Text
encodeArguments v = TE.decodeUtf8 (BL.toStrict (encode v))

toolsToOpenAI :: V.Vector Value -> [Value]
toolsToOpenAI = mapMaybe conv . V.toList
  where
    conv v = do
      o <- asObject v
      name <- lookupText "name" o
      let schema = fromMaybe (object ["type" .= ("object" :: Text)])
                             (KM.lookup "input_schema" o)
          desc   = fromMaybe "" (lookupText "description" o)
      pure $ object
        [ "type" .= ("function" :: Text)
        , "function" .= object
            [ "name" .= name
            , "description" .= desc
            , "parameters" .= schema
            ]
        ]

toolChoiceToOpenAI :: Value -> Maybe Value
toolChoiceToOpenAI v = do
  o <- asObject v
  case lookupText "type" o of
    Just "auto" -> Just (String "auto")
    Just "any"  -> Just (String "required")
    Just "none" -> Just (String "none")
    Just "tool" -> do
      n <- lookupText "name" o
      Just $ object
        [ "type" .= ("function" :: Text)
        , "function" .= object ["name" .= n]
        ]
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- OpenAI response -> Anthropic response
-- ---------------------------------------------------------------------------

-- | OpenAI stop reasons, in Anthropic's vocabulary.
mapFinishReason :: Text -> Text
mapFinishReason = \case
  "stop"           -> "end_turn"
  "length"         -> "max_tokens"
  "tool_calls"     -> "tool_use"
  "function_call"  -> "tool_use"
  "content_filter" -> "end_turn"
  other            -> other

-- | Turn a tool call's @arguments@ string into the JSON value Anthropic
-- expects, along with a warning if it did not parse.
--
-- An empty or whitespace-only string is normal (a no-argument tool) and is
-- not reported.  Anything else that fails to parse is reported rather than
-- silently discarded.
decodeToolArguments :: Text -> (Value, Maybe Text)
decodeToolArguments raw
  | T.null (T.strip raw) = (object [], Nothing)
  | otherwise = case decodeStrict (TE.encodeUtf8 raw) of
      Just v@(Object _) -> (v, Nothing)
      Just other        -> ( object []
                           , Just $ "tool arguments were valid JSON but not an object: "
                                 <> T.take 200 (TE.decodeUtf8 (BL.toStrict (encode other))) )
      Nothing           -> ( object []
                           , Just $ "tool arguments were not valid JSON: "
                                 <> T.take 200 raw )

-- | Translate a non-streaming chat completion into an Anthropic message.
openAIToAnthropicResponse :: Value -> Either Text Value
openAIToAnthropicResponse val = do
  o <- maybe (Left "response body is not a JSON object") Right (asObject val)
  -- An upstream error body has no "choices"; hand it back in Anthropic's
  -- error envelope instead of failing to translate it.
  case KM.lookup "error" o of
    Just _  -> Right (openAIErrorToAnthropic val)
    Nothing -> translateChoices o

translateChoices :: Object -> Either Text Value
translateChoices o = do
  choices <- maybe (Left "response has no \"choices\" array") Right
               (lookupArray "choices" o)
  choice  <- maybe (Left "response \"choices\" was empty") Right
               (if V.null choices then Nothing else asObject (V.head choices))
  let msg      = fromMaybe KM.empty (lookupObject "message" choice)
      textPart = case lookupText "content" msg of
        Just t | not (T.null t) -> [object ["type" .= ("text" :: Text), "text" .= t]]
        _                       -> []
      callParts =
        [ object [ "type"  .= ("tool_use" :: Text)
                 , "id"    .= callId
                 , "name"  .= fromMaybe "" (lookupText "name" fn)
                 , "input" .= fst (decodeToolArguments
                                    (fromMaybe "" (lookupText "arguments" fn)))
                 ]
        | (i, cv) <- zip [(0 :: Int) ..] (maybe [] V.toList (lookupArray "tool_calls" msg))
        , Just co <- [asObject cv]
        , let fn = fromMaybe KM.empty (lookupObject "function" co)
        , let callId = fromMaybe (T.pack ("call_" <> show i)) (lookupText "id" co)
        ]
      stop = maybe "end_turn" mapFinishReason (lookupText "finish_reason" choice)
      usage = fromMaybe KM.empty (lookupObject "usage" o)
  pure $ object
    [ "id"            .= fromMaybe "msg_proxy" (lookupText "id" o)
    , "type"          .= ("message" :: Text)
    , "role"          .= ("assistant" :: Text)
    , "model"         .= fromMaybe "" (lookupText "model" o)
    , "content"       .= (textPart <> callParts)
    , "stop_reason"   .= stop
    , "stop_sequence" .= Null
    , "usage" .= object
        [ "input_tokens"  .= fromMaybe 0 (lookupInt "prompt_tokens" usage)
        , "output_tokens" .= fromMaybe 0 (lookupInt "completion_tokens" usage)
        ]
    ]

-- | Re-shape an OpenAI-style error body into Anthropic's error envelope, so
-- an agent expecting the Anthropic shape can still show the message.
openAIErrorToAnthropic :: Value -> Value
openAIErrorToAnthropic val = object
  [ "type"  .= ("error" :: Text)
  , "error" .= object
      [ "type"    .= errType
      , "message" .= errMsg
      ]
  ]
  where
    inner = asObject val >>= lookupObject "error"
    errType = fromMaybe "api_error" (inner >>= lookupText "type")
    errMsg  = fromMaybe (TE.decodeUtf8 (BL.toStrict (encode val)))
                        (inner >>= lookupText "message")

-- ---------------------------------------------------------------------------
-- SSE plumbing
-- ---------------------------------------------------------------------------

-- | A server-sent event ready to be written to the client.
data SseEvent = SseEvent
  { sseName :: Text
  , sseData :: Value
  } deriving (Eq, Show)

renderSseEvent :: SseEvent -> ByteString
renderSseEvent (SseEvent n d) =
  "event: " <> TE.encodeUtf8 n <> "\ndata: " <> BL.toStrict (encode d) <> "\n\n"

-- | Split a byte stream into complete lines, returning the trailing partial
-- line to carry into the next chunk.  Handles both LF and CRLF.
splitSseLines :: ByteString -> ByteString -> (ByteString, [ByteString])
splitSseLines leftover chunk = go (leftover <> chunk) []
  where
    go buf acc = case BC.elemIndex '\n' buf of
      Nothing -> (buf, reverse acc)
      Just i  ->
        let line = stripCR (BS.take i buf)
            rest = BS.drop (i + 1) buf
        in go rest (line : acc)
    stripCR b
      | not (BS.null b) && BC.last b == '\r' = BS.init b
      | otherwise = b

-- ---------------------------------------------------------------------------
-- Streaming state machine
-- ---------------------------------------------------------------------------

-- | Which Anthropic content block is currently open, and what it corresponds
-- to upstream.
data OpenBlock
  = OpenText Int          -- ^ Anthropic block index.
  | OpenTool Int Int      -- ^ Anthropic block index, OpenAI tool_call index.
  deriving (Eq, Show)

blockIndex :: OpenBlock -> Int
blockIndex = \case
  OpenText i   -> i
  OpenTool i _ -> i

-- | Incremental translation of an OpenAI chat stream into an Anthropic one.
data StreamState = StreamState
  { ssStarted   :: Bool
  , ssFinished  :: Bool
  , ssMsgId     :: Text
  , ssModel     :: Text
  , ssOpen      :: Maybe OpenBlock
  , ssNextIndex :: Int
  , ssStop      :: Maybe Text
  , ssInTokens  :: Int
  , ssOutTokens :: Int
  , ssClosedTools :: [Int]
    -- ^ OpenAI @tool_calls@ indices whose Anthropic block has been closed.
    -- Anthropic content blocks cannot be reopened, so a provider that
    -- interleaves two tool calls rather than finishing one before starting
    -- the next cannot be represented faithfully.  Recording the closed ones
    -- lets that be reported instead of silently emitting a second block with
    -- the same tool id and half the arguments in each.
  } deriving (Eq, Show)

-- | @newStreamState model@ seeds the state with the model name to report if
-- upstream never names one.
newStreamState :: Text -> StreamState
newStreamState model = StreamState
  { ssStarted   = False
  , ssFinished  = False
  , ssMsgId     = "msg_proxy"
  , ssModel     = model
  , ssOpen      = Nothing
  , ssNextIndex = 0
  , ssStop      = Nothing
  , ssInTokens  = 0
  , ssOutTokens = 0
  , ssClosedTools = []
  }

-- | Whether any upstream content was seen.  The proxy uses this to tell a
-- genuinely empty stream from a connection that died before the first byte.
streamSawAnything :: StreamState -> Bool
streamSawAnything = ssStarted

-- | Feed one raw SSE line.  Returns the events to forward and any warnings
-- worth logging.
--
-- Lines that are not @data:@ payloads (comments, @event:@ names, blank
-- separators) are ignored: the Anthropic event names are generated here, not
-- copied from upstream.
stepStream :: StreamState -> ByteString -> (StreamState, [SseEvent], [Text])
stepStream st line
  | ssFinished st = (st, [], [])
  | BS.null trimmed = (st, [], [])
  | Just payload <- BC.stripPrefix "data:" trimmed =
      let body = BC.dropWhile (== ' ') payload
      in if body == "[DONE]"
           then let (st', evs) = finishStream st in (st', evs, [])
           else case decodeStrict body of
             Just (Object o) -> processChunk st o
             Just _  -> (st, [], ["ignored non-object SSE payload"])
             Nothing -> (st, [], ["could not parse SSE payload as JSON: "
                                  <> T.take 200 (TE.decodeUtf8 body)])
  | otherwise = (st, [], [])
  where
    trimmed = BC.dropWhile (== ' ') line

processChunk :: StreamState -> Object -> (StreamState, [SseEvent], [Text])
processChunk st0 chunk =
  let (st1, startEvs) = ensureStarted st0 chunk
      st2 = absorbUsage st1 chunk
      choices = maybe [] V.toList (lookupArray "choices" chunk)
  in case mapMaybe asObject (take 1 choices) of
       []       -> (st2, startEvs, [])
       (ch : _) ->
         let delta = fromMaybe KM.empty (lookupObject "delta" ch)
             (st3, tEvs, tWarn) = applyTextDelta st2 delta
             (st4, cEvs, cWarn) = applyToolDeltas st3 delta
             st5 = case lookupText "finish_reason" ch of
               Just r | not (T.null r) -> st4 { ssStop = Just (mapFinishReason r) }
               _                       -> st4
         in (st5, startEvs <> tEvs <> cEvs, tWarn <> cWarn)

-- | Emit @message_start@ exactly once, naming the upstream id and model.
ensureStarted :: StreamState -> Object -> (StreamState, [SseEvent])
ensureStarted st chunk
  | ssStarted st = (st, [])
  | otherwise =
      let st' = st { ssStarted = True
                   , ssMsgId = fromMaybe (ssMsgId st) (lookupText "id" chunk)
                   , ssModel = fromMaybe (ssModel st) (lookupText "model" chunk)
                   }
          ev = SseEvent "message_start" $ object
            [ "type" .= ("message_start" :: Text)
            , "message" .= object
                [ "id"            .= ssMsgId st'
                , "type"          .= ("message" :: Text)
                , "role"          .= ("assistant" :: Text)
                , "model"         .= ssModel st'
                , "content"       .= ([] :: [Value])
                , "stop_reason"   .= Null
                , "stop_sequence" .= Null
                , "usage" .= object
                    [ "input_tokens"  .= ssInTokens st'
                    , "output_tokens" .= (0 :: Int)
                    ]
                ]
            ]
      in (st', [ev])

-- | The usage-only chunk that @stream_options.include_usage@ produces has an
-- empty @choices@ array, so usage is absorbed independently of content.
absorbUsage :: StreamState -> Object -> StreamState
absorbUsage st chunk = case lookupObject "usage" chunk of
  Nothing -> st
  Just u  -> st { ssInTokens  = fromMaybe (ssInTokens st)  (lookupInt "prompt_tokens" u)
                , ssOutTokens = fromMaybe (ssOutTokens st) (lookupInt "completion_tokens" u)
                }

applyTextDelta :: StreamState -> Object -> (StreamState, [SseEvent], [Text])
applyTextDelta st delta =
  case firstNonEmpty [lookupText "content" delta, lookupText "refusal" delta] of
    Nothing -> (st, [], [])
    Just txt ->
      let (st1, openEvs) = ensureTextBlock st
          idx = maybe 0 blockIndex (ssOpen st1)
          ev = SseEvent "content_block_delta" $ object
            [ "type"  .= ("content_block_delta" :: Text)
            , "index" .= idx
            , "delta" .= object ["type" .= ("text_delta" :: Text), "text" .= txt]
            ]
      in (st1, openEvs <> [ev], [])
  where
    firstNonEmpty xs = case [t | Just t <- xs, not (T.null t)] of
      (t : _) -> Just t
      []      -> Nothing

ensureTextBlock :: StreamState -> (StreamState, [SseEvent])
ensureTextBlock st = case ssOpen st of
  Just (OpenText _) -> (st, [])
  _ ->
    let (st1, closeEvs) = closeOpenBlock st
        idx = ssNextIndex st1
        st2 = st1 { ssOpen = Just (OpenText idx), ssNextIndex = idx + 1 }
        ev = SseEvent "content_block_start" $ object
          [ "type"  .= ("content_block_start" :: Text)
          , "index" .= idx
          , "content_block" .= object ["type" .= ("text" :: Text), "text" .= ("" :: Text)]
          ]
    in (st2, closeEvs <> [ev])

applyToolDeltas :: StreamState -> Object -> (StreamState, [SseEvent], [Text])
applyToolDeltas st delta =
  case lookupArray "tool_calls" delta of
    Nothing -> (st, [], [])
    Just tcs -> foldl step (st, [], []) (mapMaybe asObject (V.toList tcs))
  where
    step (s, evs, warns) tc =
      let oaIndex = fromMaybe 0 (lookupInt "index" tc)
          fn      = fromMaybe KM.empty (lookupObject "function" tc)
          -- Pass the upstream call id through verbatim so that the tool_result
          -- the agent sends back carries an id OpenAI will recognise.
          callId  = fromMaybe (T.pack ("call_" <> show oaIndex)) (lookupText "id" tc)
          name    = fromMaybe "" (lookupText "name" fn)
          (s1, openEvs, openWarn) = ensureToolBlock s oaIndex callId name
          argEvs = case lookupText "arguments" fn of
            Just a | not (T.null a) ->
              [ SseEvent "content_block_delta" $ object
                  [ "type"  .= ("content_block_delta" :: Text)
                  , "index" .= maybe 0 blockIndex (ssOpen s1)
                  , "delta" .= object
                      [ "type" .= ("input_json_delta" :: Text)
                      , "partial_json" .= a
                      ]
                  ] ]
            _ -> []
      in (s1, evs <> openEvs <> argEvs, warns <> openWarn)

ensureToolBlock
  :: StreamState -> Int -> Text -> Text -> (StreamState, [SseEvent], [Text])
ensureToolBlock st oaIndex callId name = case ssOpen st of
  Just (OpenTool _ i) | i == oaIndex -> (st, [], [])
  _ ->
    let (st1, closeEvs) = closeOpenBlock st
        idx = ssNextIndex st1
        st2 = st1 { ssOpen = Just (OpenTool idx oaIndex), ssNextIndex = idx + 1 }
        ev = SseEvent "content_block_start" $ object
          [ "type"  .= ("content_block_start" :: Text)
          , "index" .= idx
          , "content_block" .= object
              [ "type"  .= ("tool_use" :: Text)
              , "id"    .= callId
              , "name"  .= name
              , "input" .= object []
              ]
          ]
        warn =
          [ "upstream resumed tool call " <> T.pack (show oaIndex)
            <> " (id " <> callId <> ") after its content block was closed; "
            <> "its arguments will arrive split across two tool_use blocks"
          | oaIndex `elem` ssClosedTools st1 ]
    in (st2, closeEvs <> [ev], warn)

closeOpenBlock :: StreamState -> (StreamState, [SseEvent])
closeOpenBlock st = case ssOpen st of
  Nothing -> (st, [])
  Just b  ->
    ( st { ssOpen = Nothing
         , ssClosedTools = case b of
             OpenTool _ oaIndex -> oaIndex : ssClosedTools st
             OpenText _         -> ssClosedTools st
         }
    , [ SseEvent "content_block_stop" $ object
          [ "type" .= ("content_block_stop" :: Text)
          , "index" .= blockIndex b
          ] ] )

-- | Close the stream.  Idempotent, and safe to call on a stream that never
-- produced a chunk -- an agent still receives a well-formed, empty message
-- rather than a truncated one.
finishStream :: StreamState -> (StreamState, [SseEvent])
finishStream st
  | ssFinished st = (st, [])
  | otherwise =
      let (st1, startEvs) = if ssStarted st then (st, []) else ensureStarted st KM.empty
          (st2, closeEvs) = closeOpenBlock st1
          stop = fromMaybe "end_turn" (ssStop st2)
          deltaEv = SseEvent "message_delta" $ object
            [ "type"  .= ("message_delta" :: Text)
            , "delta" .= object
                [ "stop_reason"   .= stop
                , "stop_sequence" .= Null
                ]
            -- Both counts, not just the output one.  OpenAI reports usage in
            -- a trailing chunk, long after @message_start@ has gone out with
            -- the only input count it could know at the time: zero.  If the
            -- real prompt count is not repeated here it is simply lost, and
            -- an agent that displays or bills on token usage reports no input
            -- tokens at all -- the larger half of the cost.  @input_tokens@
            -- is a documented field of the @message_delta@ usage object and
            -- is cumulative, which is exactly what this is.
            , "usage" .= object
                [ "input_tokens"  .= ssInTokens st2
                , "output_tokens" .= ssOutTokens st2
                ]
            ]
          stopEv = SseEvent "message_stop" $
            object ["type" .= ("message_stop" :: Text)]
      in (st2 { ssFinished = True }, startEvs <> closeEvs <> [deltaEv, stopEv])
