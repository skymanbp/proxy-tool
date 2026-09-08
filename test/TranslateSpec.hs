{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the rewriting and translation layer.
--
-- These are the parts where a mistake is silent: a dropped tool result or a
-- malformed event stream shows up as an agent behaving oddly, not as a crash.
module TranslateSpec (spec) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

import ProxyTool.Translate
import ProxyTool.Types

spec :: Spec
spec = do
  requestSpec
  responseSpec
  sseSpec
  streamSpec
  rewriteSpec

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

opts :: TranslateOpts
opts = defaultTranslateOpts

at :: Key -> Value -> Maybe Value
at k (Object o) = KM.lookup k o
at _ _          = Nothing

arr :: Value -> [Value]
arr (Array a) = V.toList a
arr _         = []

textAt :: Key -> Value -> Maybe Text
textAt k v = case at k v of
  Just (String s) -> Just s
  _               -> Nothing

-- | Translate and blow up loudly if it refused, so a failing test names the
-- reason instead of just mismatching.
mustTranslate :: TranslateOpts -> Value -> Value
mustTranslate o v = case anthropicToOpenAIRequest o v of
  Left err -> error ("translation failed: " <> show err)
  Right x  -> x

messagesOf :: Value -> [Value]
messagesOf = arr . fromMaybe Null . at "messages"

-- | Total replacements for @head@ and irrefutable @let@ patterns.  A test
-- that trips one of these reports what it expected instead of dying inside
-- Prelude with no context.
firstOf :: String -> [a] -> a
firstOf what []      = error ("expected at least one " <> what <> ", got none")
firstOf _    (x : _) = x

onlyOf :: String -> [a] -> a
onlyOf what xs = case xs of
  [x] -> x
  _   -> error ("expected exactly one " <> what <> ", got "
                <> show (length xs) <> " of them")

rightOf :: Either Text a -> a
rightOf (Right a) = a
rightOf (Left e)  = error ("expected success, got: " <> show e)

numberAt :: Key -> Value -> Double
numberAt k v = case at k v of
  Just (Number n) -> toRealFloat n
  other           -> error ("expected a number at " <> show k
                            <> ", got " <> show other)

-- ---------------------------------------------------------------------------
-- Anthropic request -> OpenAI request
-- ---------------------------------------------------------------------------

requestSpec :: Spec
requestSpec = describe "anthropicToOpenAIRequest" $ do

  it "rejects a body that is not an object" $
    anthropicToOpenAIRequest opts (String "nope")
      `shouldSatisfy` either (const True) (const False)

  it "rejects a body with no messages" $
    anthropicToOpenAIRequest opts (object ["model" .= ("m" :: Text)])
      `shouldSatisfy` either (const True) (const False)

  it "moves a string system prompt into a leading system message" $ do
    let out = mustTranslate opts $ object
          [ "system"   .= ("be terse" :: Text)
          , "messages" .= [object ["role" .= ("user" :: Text), "content" .= ("hi" :: Text)]]
          ]
        ms = messagesOf out
    length ms `shouldBe` 2
    textAt "role" (firstOf "message" ms) `shouldBe` Just "system"
    textAt "content" (firstOf "message" ms) `shouldBe` Just "be terse"

  it "flattens a block-form system prompt" $ do
    let out = mustTranslate opts $ object
          [ "system" .= [ object ["type" .= ("text" :: Text), "text" .= ("a" :: Text)]
                        , object ["type" .= ("text" :: Text), "text" .= ("b" :: Text)] ]
          , "messages" .= ([] :: [Value])
          ]
    textAt "content" (firstOf "message" (messagesOf out)) `shouldBe` Just "a\n\nb"

  it "omits the system message entirely when there is none" $ do
    let out = mustTranslate opts $ object
          [ "messages" .= [object ["role" .= ("user" :: Text), "content" .= ("hi" :: Text)]] ]
    map (textAt "role") (messagesOf out) `shouldBe` [Just "user"]

  it "turns an assistant tool_use block into tool_calls with string arguments" $ do
    let out = mustTranslate opts $ object
          [ "messages" .=
              [ object
                  [ "role" .= ("assistant" :: Text)
                  , "content" .=
                      [ object ["type" .= ("text" :: Text), "text" .= ("working" :: Text)]
                      , object [ "type" .= ("tool_use" :: Text)
                               , "id" .= ("call_7" :: Text)
                               , "name" .= ("read_file" :: Text)
                               , "input" .= object ["path" .= ("a.txt" :: Text)]
                               ]
                      ]
                  ] ] ]
        m = firstOf "message" (messagesOf out)
        calls = arr (fromMaybe Null (at "tool_calls" m))
    textAt "content" m `shouldBe` Just "working"
    length calls `shouldBe` 1
    textAt "id" (firstOf "tool call" calls) `shouldBe` Just "call_7"
    textAt "type" (firstOf "tool call" calls) `shouldBe` Just "function"
    let fn = fromMaybe Null (at "function" (firstOf "tool call" calls))
    textAt "name" fn `shouldBe` Just "read_file"
    -- OpenAI wants the arguments as a JSON *string*, not an object.
    textAt "arguments" fn `shouldBe` Just "{\"path\":\"a.txt\"}"

  it "splits tool_result blocks out of the user turn into tool messages" $ do
    let out = mustTranslate opts $ object
          [ "messages" .=
              [ object
                  [ "role" .= ("user" :: Text)
                  , "content" .=
                      [ object [ "type" .= ("tool_result" :: Text)
                               , "tool_use_id" .= ("call_7" :: Text)
                               , "content" .= ("file contents" :: Text)
                               ]
                      , object ["type" .= ("text" :: Text), "text" .= ("now what" :: Text)]
                      ]
                  ] ] ]
        ms = messagesOf out
    map (textAt "role") ms `shouldBe` [Just "tool", Just "user"]
    textAt "tool_call_id" (firstOf "message" ms) `shouldBe` Just "call_7"
    textAt "content" (firstOf "message" ms) `shouldBe` Just "file contents"

  it "keeps the tool message ahead of the user text, as the schema requires" $ do
    -- Order matters: a tool message must follow the assistant turn that
    -- requested it, so it cannot drift behind sibling user content.
    let out = mustTranslate opts $ object
          [ "messages" .=
              [ object
                  [ "role" .= ("user" :: Text)
                  , "content" .=
                      [ object ["type" .= ("text" :: Text), "text" .= ("hello" :: Text)]
                      , object [ "type" .= ("tool_result" :: Text)
                               , "tool_use_id" .= ("call_1" :: Text)
                               , "content" .= ("done" :: Text) ]
                      ]
                  ] ] ]
    map (textAt "role") (messagesOf out) `shouldBe` [Just "tool", Just "user"]

  it "flattens a block-form tool_result into text" $ do
    let out = mustTranslate opts $ object
          [ "messages" .=
              [ object
                  [ "role" .= ("user" :: Text)
                  , "content" .=
                      [ object [ "type" .= ("tool_result" :: Text)
                               , "tool_use_id" .= ("c" :: Text)
                               , "content" .=
                                   [ object ["type" .= ("text" :: Text), "text" .= ("l1" :: Text)]
                                   , object ["type" .= ("text" :: Text), "text" .= ("l2" :: Text)] ]
                               ] ]
                  ] ] ]
    textAt "content" (firstOf "message" (messagesOf out)) `shouldBe` Just "l1\nl2"

  it "re-expresses a base64 image as a data URL" $ do
    let out = mustTranslate opts $ object
          [ "messages" .=
              [ object
                  [ "role" .= ("user" :: Text)
                  , "content" .=
                      [ object [ "type" .= ("image" :: Text)
                               , "source" .= object
                                   [ "type" .= ("base64" :: Text)
                                   , "media_type" .= ("image/png" :: Text)
                                   , "data" .= ("QUJD" :: Text) ] ] ]
                  ] ] ]
        parts = arr (fromMaybe Null (at "content" (firstOf "message" (messagesOf out))))
    textAt "type" (firstOf "content part" parts) `shouldBe` Just "image_url"
    textAt "url" (fromMaybe Null (at "image_url" (firstOf "content part" parts)))
      `shouldBe` Just "data:image/png;base64,QUJD"

  it "converts tools to the function-calling shape" $ do
    let out = mustTranslate opts $ object
          [ "messages" .= ([] :: [Value])
          , "tools" .=
              [ object [ "name" .= ("grep" :: Text)
                       , "description" .= ("search" :: Text)
                       , "input_schema" .= object ["type" .= ("object" :: Text)] ] ]
          ]
        t = firstOf "tool" (arr (fromMaybe Null (at "tools" out)))
    textAt "type" t `shouldBe` Just "function"
    let fn = fromMaybe Null (at "function" t)
    textAt "name" fn `shouldBe` Just "grep"
    at "parameters" fn `shouldBe` Just (object ["type" .= ("object" :: Text)])

  it "maps every tool_choice form" $ do
    let choiceFor c = at "tool_choice" $ mustTranslate opts $ object
          [ "messages" .= ([] :: [Value]), "tool_choice" .= c ]
    choiceFor (object ["type" .= ("auto" :: Text)]) `shouldBe` Just (String "auto")
    choiceFor (object ["type" .= ("any" :: Text)])  `shouldBe` Just (String "required")
    choiceFor (object ["type" .= ("none" :: Text)]) `shouldBe` Just (String "none")
    choiceFor (object ["type" .= ("tool" :: Text), "name" .= ("f" :: Text)])
      `shouldBe` Just (object [ "type" .= ("function" :: Text)
                              , "function" .= object ["name" .= ("f" :: Text)] ])

  it "carries max_tokens, stop sequences and sampling across" $ do
    let out = mustTranslate opts $ object
          [ "messages" .= ([] :: [Value])
          , "max_tokens" .= (321 :: Int)
          , "stop_sequences" .= (["STOP"] :: [Text])
          , "temperature" .= (0.5 :: Double)
          , "top_p" .= (0.9 :: Double)
          ]
    at "max_tokens" out  `shouldBe` Just (toJSON (321 :: Int))
    at "stop" out        `shouldBe` Just (toJSON (["STOP"] :: [Text]))
    at "temperature" out `shouldBe` Just (toJSON (0.5 :: Double))
    at "top_p" out       `shouldBe` Just (toJSON (0.9 :: Double))

  it "asks for usage only when streaming" $ do
    let streamed = mustTranslate opts $ object
          [ "messages" .= ([] :: [Value]), "stream" .= True ]
        plain = mustTranslate opts $ object [ "messages" .= ([] :: [Value]) ]
    at "stream_options" streamed
      `shouldBe` Just (object ["include_usage" .= True])
    at "stream_options" plain `shouldBe` Nothing

  it "forces the configured model and effort" $ do
    let o = opts { optModel = Just "kimi-k2", optEffort = Just EffortHigh }
        out = mustTranslate o $ object
          [ "model" .= ("claude-opus-4-5" :: Text), "messages" .= ([] :: [Value]) ]
    textAt "model" out `shouldBe` Just "kimi-k2"
    textAt "reasoning_effort" out `shouldBe` Just "high"

  it "clamps a Codex-only effort down to the documented OpenAI range" $ do
    let o = opts { optModel = Just "m", optEffort = Just EffortUltra }
        out = mustTranslate o (object ["messages" .= ([] :: [Value])])
    textAt "reasoning_effort" out `shouldBe` Just "xhigh"

  it "substitutes the fast model only for a fast-tier request" $ do
    let o = opts { optModel = Just "big", optFastModel = Just "small" }
        run m = textAt "model" $ mustTranslate o $ object
          [ "model" .= (m :: Text), "messages" .= ([] :: [Value]) ]
    run "claude-3-5-haiku-20241022" `shouldBe` Just "small"
    run "claude-opus-4-5"           `shouldBe` Just "big"

  it "lets the provider's extra body win over everything else" $ do
    let o = opts { optModel = Just "a"
                 , optExtraBody = Just (KM.fromList [("model", String "override")]) }
        out = mustTranslate o (object ["messages" .= ([] :: [Value])])
    textAt "model" out `shouldBe` Just "override"

-- ---------------------------------------------------------------------------
-- OpenAI response -> Anthropic response
-- ---------------------------------------------------------------------------

responseSpec :: Spec
responseSpec = describe "openAIToAnthropicResponse" $ do

  it "converts text content and usage" $ do
    let out = rightOf $ openAIToAnthropicResponse $ object
          [ "id" .= ("chatcmpl-1" :: Text)
          , "model" .= ("gpt-x" :: Text)
          , "choices" .=
              [ object [ "index" .= (0 :: Int)
                       , "message" .= object [ "role" .= ("assistant" :: Text)
                                             , "content" .= ("hello" :: Text) ]
                       , "finish_reason" .= ("stop" :: Text) ] ]
          , "usage" .= object [ "prompt_tokens" .= (11 :: Int)
                              , "completion_tokens" .= (22 :: Int) ]
          ]
    textAt "type" out        `shouldBe` Just "message"
    textAt "role" out        `shouldBe` Just "assistant"
    textAt "stop_reason" out `shouldBe` Just "end_turn"
    let blocks = arr (fromMaybe Null (at "content" out))
    map (textAt "type") blocks `shouldBe` [Just "text"]
    textAt "text" (firstOf "content block" blocks) `shouldBe` Just "hello"
    at "input_tokens"  (fromMaybe Null (at "usage" out)) `shouldBe` Just (toJSON (11 :: Int))
    at "output_tokens" (fromMaybe Null (at "usage" out)) `shouldBe` Just (toJSON (22 :: Int))

  it "converts tool calls back into tool_use blocks with parsed input" $ do
    let out = rightOf $ openAIToAnthropicResponse $ object
          [ "choices" .=
              [ object
                  [ "message" .= object
                      [ "tool_calls" .=
                          [ object [ "id" .= ("call_9" :: Text)
                                   , "function" .= object
                                       [ "name" .= ("edit" :: Text)
                                       , "arguments" .= ("{\"n\":2}" :: Text) ] ] ] ]
                  , "finish_reason" .= ("tool_calls" :: Text) ] ] ]
        blocks = arr (fromMaybe Null (at "content" out))
    textAt "stop_reason" out `shouldBe` Just "tool_use"
    textAt "type" (firstOf "content block" blocks) `shouldBe` Just "tool_use"
    textAt "id"   (firstOf "content block" blocks) `shouldBe` Just "call_9"
    at "input" (firstOf "content block" blocks) `shouldBe` Just (object ["n" .= (2 :: Int)])

  it "maps each finish reason" $ do
    map mapFinishReason ["stop", "length", "tool_calls", "function_call", "content_filter"]
      `shouldBe` ["end_turn", "max_tokens", "tool_use", "tool_use", "end_turn"]

  it "re-shapes an upstream error instead of failing to translate it" $ do
    let out = rightOf $ openAIToAnthropicResponse $ object
          [ "error" .= object [ "type" .= ("invalid_request_error" :: Text)
                              , "message" .= ("bad model" :: Text) ] ]
    textAt "type" out `shouldBe` Just "error"
    textAt "message" (fromMaybe Null (at "error" out)) `shouldBe` Just "bad model"

  it "reports a response with no choices rather than inventing one" $
    openAIToAnthropicResponse (object ["choices" .= ([] :: [Value])])
      `shouldSatisfy` either (const True) (const False)

describeArgs :: Text -> (Value, Maybe Text)
describeArgs = decodeToolArguments

-- ---------------------------------------------------------------------------
-- Tool argument decoding
-- ---------------------------------------------------------------------------

sseSpec :: Spec
sseSpec = describe "tool arguments and SSE framing" $ do

  it "treats empty tool arguments as a no-argument call, without warning" $
    describeArgs "  " `shouldBe` (object [], Nothing)

  it "warns rather than silently discarding unparsable arguments" $ do
    let (v, w) = describeArgs "{not json"
    v `shouldBe` object []
    w `shouldSatisfy` maybe False (const True)

  it "reassembles a line split across two chunks" $ do
    let (leftover1, lines1) = splitSseLines "" "data: {\"a\":"
        (leftover2, lines2) = splitSseLines leftover1 "1}\ndata: x\n"
    lines1    `shouldBe` []
    leftover1 `shouldBe` "data: {\"a\":"
    lines2    `shouldBe` ["data: {\"a\":1}", "data: x"]
    leftover2 `shouldBe` ""

  it "strips CR so CRLF streams parse" $
    snd (splitSseLines "" "data: a\r\ndata: b\r\n")
      `shouldBe` ["data: a", "data: b"]

-- ---------------------------------------------------------------------------
-- Streaming translation
-- ---------------------------------------------------------------------------

-- | Drive the state machine over a list of raw SSE lines and collect the
-- Anthropic events it produces, closing the stream at the end.
runStream :: Text -> [ByteString] -> [SseEvent]
runStream model ls =
  let (stEnd, evs) = foldl step (newStreamState model, []) ls
      (_, closing) = finishStream stEnd
  in evs <> closing
  where
    step (st, acc) l = let (st', es, _) = stepStream st l in (st', acc <> es)

names :: [SseEvent] -> [Text]
names = map sseName

streamSpec :: Spec
streamSpec = describe "stepStream / finishStream" $ do

  it "emits a well-formed envelope for a plain text stream" $ do
    let evs = runStream "m"
          [ "data: {\"id\":\"chatcmpl-1\",\"model\":\"gpt-x\",\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"\"}}]}"
          , "data: {\"choices\":[{\"delta\":{\"content\":\"Hel\"}}]}"
          , "data: {\"choices\":[{\"delta\":{\"content\":\"lo\"}}]}"
          , "data: {\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
          , "data: [DONE]"
          ]
    names evs `shouldBe`
      [ "message_start", "content_block_start"
      , "content_block_delta", "content_block_delta"
      , "content_block_stop", "message_delta", "message_stop" ]

  it "carries the upstream id and model into message_start" $ do
    let e = firstOf "event" $ runStream "fallback"
          [ "data: {\"id\":\"chatcmpl-9\",\"model\":\"real-model\",\"choices\":[{\"delta\":{\"content\":\"x\"}}]}" ]
        msg = fromMaybe Null (at "message" (sseData e))
    textAt "id" msg    `shouldBe` Just "chatcmpl-9"
    textAt "model" msg `shouldBe` Just "real-model"

  it "does not open a text block for an empty content delta" $ do
    let evs = runStream "m"
          [ "data: {\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"\"}}]}"
          , "data: [DONE]" ]
    names evs `shouldBe` ["message_start", "message_delta", "message_stop"]

  it "translates a tool call into a tool_use block with input_json_delta" $ do
    let evs = runStream "m"
          [ "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"function\":{\"name\":\"grep\",\"arguments\":\"\"}}]}}]}"
          , "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\\\"q\\\":\"}}]}}]}"
          , "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"1}\"}}]}}]}"
          , "data: {\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
          , "data: [DONE]" ]
    names evs `shouldBe`
      [ "message_start", "content_block_start"
      , "content_block_delta", "content_block_delta"
      , "content_block_stop", "message_delta", "message_stop" ]
    let start = evs !! 1
        block = fromMaybe Null (at "content_block" (sseData start))
    textAt "type" block `shouldBe` Just "tool_use"
    -- The upstream id passes through unchanged so the tool_result the agent
    -- sends back carries an id the provider will recognise.
    textAt "id"   block `shouldBe` Just "call_1"
    textAt "name" block `shouldBe` Just "grep"
    let d = fromMaybe Null (at "delta" (sseData (evs !! 2)))
    textAt "type" d `shouldBe` Just "input_json_delta"
    textAt "partial_json" d `shouldBe` Just "{\"q\":"

  it "closes the text block before opening a tool block" $ do
    let evs = runStream "m"
          [ "data: {\"choices\":[{\"delta\":{\"content\":\"thinking\"}}]}"
          , "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c1\",\"function\":{\"name\":\"f\",\"arguments\":\"{}\"}}]}}]}"
          , "data: [DONE]" ]
    names evs `shouldBe`
      [ "message_start", "content_block_start", "content_block_delta"
      , "content_block_stop"           -- text block closed ...
      , "content_block_start"          -- ... before the tool block opens
      , "content_block_delta", "content_block_stop"
      , "message_delta", "message_stop" ]

  it "gives consecutive blocks distinct indices" $ do
    let evs = runStream "m"
          [ "data: {\"choices\":[{\"delta\":{\"content\":\"a\"}}]}"
          , "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c\",\"function\":{\"name\":\"f\"}}]}}]}"
          , "data: [DONE]" ]
        idx = [ v | e <- evs, sseName e == "content_block_start"
                  , Just v <- [at "index" (sseData e)] ]
    idx `shouldBe` [toJSON (0 :: Int), toJSON (1 :: Int)]

  it "absorbs the trailing usage-only chunk, which has no choices" $ do
    -- Both halves of the count have to survive.  OpenAI reports usage only at
    -- the end, by which time message_start has already claimed zero input
    -- tokens, so message_delta is the one chance to tell the agent what the
    -- prompt actually cost.  Asserting only on output_tokens is what let that
    -- go unnoticed.
    let evs = runStream "m"
          [ "data: {\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}"
          , "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":7}}"
          , "data: [DONE]" ]
        delta = onlyOf "message_delta" [e | e <- evs, sseName e == "message_delta"]
        usage = fromMaybe Null (at "usage" (sseData delta))
    at "input_tokens"  usage `shouldBe` Just (toJSON (5 :: Int))
    at "output_tokens" usage `shouldBe` Just (toJSON (7 :: Int))

  it "reports the mapped stop reason" $ do
    let evs = runStream "m"
          [ "data: {\"choices\":[{\"delta\":{\"content\":\"x\"},\"finish_reason\":\"length\"}]}"
          , "data: [DONE]" ]
        delta = onlyOf "message_delta" [e | e <- evs, sseName e == "message_delta"]
    textAt "stop_reason" (fromMaybe Null (at "delta" (sseData delta)))
      `shouldBe` Just "max_tokens"

  it "still produces a complete message when upstream sends nothing" $
    -- A truncated upstream must not leave the agent waiting on a stream that
    -- never terminates.
    names (runStream "m" []) `shouldBe`
      ["message_start", "message_delta", "message_stop"]

  it "ignores comments and event: lines" $
    names (runStream "m" [": keep-alive", "event: whatever", ""])
      `shouldBe` ["message_start", "message_delta", "message_stop"]

  it "is idempotent after [DONE]" $ do
    let evs = runStream "m" ["data: [DONE]", "data: {\"choices\":[{\"delta\":{\"content\":\"late\"}}]}"]
    names evs `shouldBe` ["message_start", "message_delta", "message_stop"]

  it "renders an event in SSE framing" $
    renderSseEvent (SseEvent "message_stop" (object ["type" .= ("message_stop" :: Text)]))
      `shouldBe` "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"

-- ---------------------------------------------------------------------------
-- Same-dialect rewriting
-- ---------------------------------------------------------------------------

rewriteSpec :: Spec
rewriteSpec = describe "same-dialect rewriting" $ do

  it "forces the model on an Anthropic request" $ do
    let out = rewriteAnthropicRequest (opts { optModel = Just "kimi" }) $
                object ["model" .= ("claude-x" :: Text)]
    textAt "model" out `shouldBe` Just "kimi"

  it "turns effort into a thinking budget" $ do
    let out = rewriteAnthropicRequest (opts { optEffort = Just EffortHigh }) $
                object ["max_tokens" .= (64000 :: Int)]
        th = fromMaybe Null (at "thinking" out)
    textAt "type" th `shouldBe` Just "enabled"
    at "budget_tokens" th `shouldBe` Just (toJSON (24576 :: Int))

  it "raises max_tokens so the budget stays below it" $ do
    -- The API requires budget_tokens < max_tokens; a small max_tokens must
    -- not produce a request the provider will reject.
    let out = rewriteAnthropicRequest (opts { optEffort = Just EffortHigh }) $
                object ["max_tokens" .= (1000 :: Int)]
        maxTok = numberAt "max_tokens" out
        budget = numberAt "budget_tokens" (fromMaybe Null (at "thinking" out))
    budget `shouldSatisfy` (< maxTok)
    budget `shouldSatisfy` (>= 1024)

  it "drops sampling parameters that cannot accompany thinking" $ do
    let out = rewriteAnthropicRequest (opts { optEffort = Just EffortHigh }) $
                object [ "max_tokens" .= (64000 :: Int)
                       , "temperature" .= (0.7 :: Double)
                       , "top_p" .= (0.9 :: Double)
                       , "top_k" .= (5 :: Int) ]
    at "temperature" out `shouldBe` Nothing
    at "top_p" out       `shouldBe` Nothing
    at "top_k" out       `shouldBe` Nothing

  it "leaves sampling alone when no effort is configured" $ do
    let out = rewriteAnthropicRequest opts $
                object ["temperature" .= (0.7 :: Double)]
    at "temperature" out `shouldBe` Just (toJSON (0.7 :: Double))

  it "does not enable thinking for effort none" $ do
    let out = rewriteAnthropicRequest (opts { optEffort = Just EffortNone }) $
                object ["max_tokens" .= (4096 :: Int)]
    at "thinking" out `shouldBe` Nothing

  it "sets reasoning_effort on an OpenAI request" $ do
    let out = rewriteOpenAIRequest (opts { optEffort = Just EffortMedium }) $
                object ["model" .= ("m" :: Text)]
    textAt "reasoning_effort" out `shouldBe` Just "medium"
