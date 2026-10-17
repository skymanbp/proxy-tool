{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The pure half of @proxy-tool serve@: which endpoint a request is for, and
-- the answers the proxy computes itself instead of forwarding.
--
-- A launched session ('ProxyTool.Session.runSession') forwards every request
-- to the provider and lets the agent cope with whatever comes back.  A
-- standing proxy that another program supervises needs a little more: a
-- health endpoint the supervisor can poll, and correct answers for the two
-- side calls Claude Code makes besides @POST \/v1\/messages@ -- counting
-- tokens and listing models -- which an OpenAI-compatible provider has no
-- equivalent for at those paths.
--
-- Everything here is total and has no IO, so the decisions are tested
-- directly; "ProxyTool.Proxy" does the networking around them.
module ProxyTool.ServeRoutes
  ( -- * Routing
    ServeRoute (..)
  , classifyServeRoute
    -- * Standing-proxy facts
  , ServeInfo (..)
  , healthzBody
  , tokenMatches
    -- * Token counting
  , estimateInputTokens
  , countTokensBody
    -- * Model listing
  , openAIModelsToAnthropic
  , configuredModelList
  , modelListMissing
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (xor, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V

-- ---------------------------------------------------------------------------
-- Routing
-- ---------------------------------------------------------------------------

-- | The endpoints a standing proxy distinguishes.  Anything else is a 404:
-- forwarding an unknown path to the provider's chat endpoint is how the old
-- single-path proxy turned a token count into a bogus completion.
data ServeRoute
  = ServeHealthz      -- ^ @GET \/healthz@
  | ServeMessages     -- ^ @POST \/v1\/messages@
  | ServeCountTokens  -- ^ @POST \/v1\/messages\/count_tokens@
  | ServeModels       -- ^ @GET \/v1\/models@
  | ServeShutdown     -- ^ @POST \/admin\/shutdown@ (token required)
  | ServeNotFound
  deriving (Eq, Show)

-- | Decide the endpoint from the method and the decoded path segments (wai's
-- @pathInfo@, so the query string -- Claude Code sends @?beta=true@ -- is
-- already gone).
classifyServeRoute :: ByteString -> [Text] -> ServeRoute
classifyServeRoute method segments = case (method, segments) of
  ("GET",  ["healthz"])                         -> ServeHealthz
  ("POST", ["v1", "messages"])                  -> ServeMessages
  ("POST", ["v1", "messages", "count_tokens"])  -> ServeCountTokens
  ("GET",  ["v1", "models"])                    -> ServeModels
  ("POST", ["admin", "shutdown"])               -> ServeShutdown
  _                                             -> ServeNotFound

-- ---------------------------------------------------------------------------
-- Health
-- ---------------------------------------------------------------------------

-- | What a standing proxy knows about itself.
--
-- The identity fields are the provider entry as it was LOADED at start: a
-- standing proxy forces that model, effort and base URL on every request for
-- as long as it runs, so a supervisor that has since changed the entry must
-- be able to see that this process still serves the old one (and restart
-- it) rather than have the change silently ignored.
data ServeInfo = ServeInfo
  { siProvider  :: Text
    -- ^ The provider id it fronts, as named on the command line.
  , siPort      :: Int
    -- ^ The port it is bound to.
  , siStarted   :: Double
    -- ^ Monotonic start time in seconds ('GHC.Clock.getMonotonicTime').
  , siModel     :: Maybe Text
    -- ^ The model it forces ('Nothing': the client's model is left alone).
  , siEffort    :: Maybe Text
    -- ^ The reasoning effort it injects, as its slug.
  , siBaseUrl   :: Text
    -- ^ The provider base URL it forwards to.
  , siStartedAt :: Text
    -- ^ Wall-clock start time, ISO 8601 UTC.
  , siPid       :: Int
    -- ^ This process's id.
  } deriving (Eq, Show)

-- | Whether a presented token equals the proxy's own, in time that depends
-- only on the two lengths and never on where the first differing byte is
-- (every byte pair is XOR-ed and OR-ed together; nothing stops early).
tokenMatches :: Text -> Text -> Bool
tokenMatches expected presented =
  let e = TE.encodeUtf8 expected
      p = TE.encodeUtf8 presented
      diff = BS.foldl' (.|.) 0 (BS.packZipWith xor e p)
  in BS.length e == BS.length p && diff == 0

-- | The @GET \/healthz@ answer.
--
-- The endpoint needs no token, so a supervisor can tell \"nothing is
-- listening\" from \"something is listening but rejects me\".  When the caller
-- does present a token, the answer says whether it is the right one
-- (@token_ok@): a proxy started under an older token is up, but useless to
-- the caller, and only the caller can tell those two apart.  The token itself
-- is never echoed, and neither is the provider's credential: the identity
-- is the loaded entry's model, effort and base URL, plus when and as which
-- process the proxy started.
healthzBody :: ServeInfo -> Double -> Maybe Bool -> Value
healthzBody si now tokenOk = object $
  [ "ok"         .= True
  , "provider"   .= siProvider si
  , "model"      .= siModel si
  , "effort"     .= siEffort si
  , "base_url"   .= siBaseUrl si
  , "port"       .= siPort si
  , "started_at" .= siStartedAt si
  , "pid"        .= siPid si
  , "uptime_s"   .= (max 0 (floor (now - siStarted si)) :: Int)
  ] <> maybe [] (\b -> ["token_ok" .= b]) tokenOk

-- ---------------------------------------------------------------------------
-- Token counting
-- ---------------------------------------------------------------------------

-- | Estimate the input tokens of an Anthropic Messages request.
--
-- Formula: take the request's @system@, @messages@ and @tools@ fields -- the
-- parts that become prompt tokens -- encode them as compact JSON, count the
-- characters (Unicode code points, not bytes) and divide by four, rounding
-- up.  Four characters per token is the usual rule of thumb for English text
-- and code; it overestimates CJK text, which errs on the side Claude Code
-- uses the number for (deciding when to compact).  It is an estimate on
-- purpose: an OpenAI-compatible provider offers no counting endpoint, and
-- asking it for a completion to learn the count would bill a request the
-- user never made.
estimateInputTokens :: Value -> Either Text Int
estimateInputTokens = \case
  Object o ->
    let parts = KM.fromList
          [ (k, v) | k <- ["system", "messages", "tools"]
                   , Just v <- [KM.lookup k o], v /= Null ]
        chars = T.length (TE.decodeUtf8 (BL.toStrict (encode (Object parts))))
    in Right ((chars + 3) `div` 4)
  _ -> Left "count_tokens body must be a JSON object"

-- | The Anthropic @count_tokens@ response shape.
countTokensBody :: Int -> Value
countTokensBody n = object ["input_tokens" .= n]

-- ---------------------------------------------------------------------------
-- Model listing
-- ---------------------------------------------------------------------------

-- | Translate an OpenAI @GET \/models@ answer (@{data: [{id, created?}]}@) to
-- Anthropic's list shape (@{data: [{type, id, display_name, created_at?}],
-- has_more, first_id, last_id}@).
--
-- @created_at@ is derived from OpenAI's @created@ (Unix seconds) when the
-- provider sends it, and left out when it does not: a made-up date would be a
-- fact nobody measured.
openAIModelsToAnthropic :: Value -> Either Text Value
openAIModelsToAnthropic = \case
  Object o | Just (Array xs) <- KM.lookup "data" o ->
    Right (anthropicList (mapMaybe entry (V.toList xs)))
  _ -> Left "provider model list has no \"data\" array"
  where
    entry = \case
      Object m | Just (String mid) <- KM.lookup "id" m -> Just (mid, created m)
      _ -> Nothing
    created m = case KM.lookup "created" m of
      Just (Number n) -> toBoundedInteger n :: Maybe Int
      _               -> Nothing

-- | The list a provider without a model-list endpoint is described by: the
-- configured model alone, or nothing when no model is configured.
configuredModelList :: Maybe Text -> Value
configuredModelList = \case
  Just m | not (T.null (T.strip m)) -> anthropicList [(m, Nothing)]
  _                                 -> anthropicList []

-- | The upstream statuses that mean \"this provider has no model-list
-- endpoint\" rather than \"the request failed\".  Every other non-2xx status
-- is passed through untouched.
modelListMissing :: Int -> Bool
modelListMissing s = s == 404 || s == 405

anthropicList :: [(Text, Maybe Int)] -> Value
anthropicList ms = object
  [ "data"     .= map one ms
  , "has_more" .= False
  , "first_id" .= fmap fst (safeHead ms)
  , "last_id"  .= fmap fst (safeHead (reverse ms))
  ]
  where
    one (mid, c) = object $
      [ "type"         .= ("model" :: Text)
      , "id"           .= mid
      , "display_name" .= mid
      ] <> maybe [] (\s -> ["created_at" .= iso (fromIntegral s)]) c
    iso :: Integer -> Text
    iso s = T.pack (iso8601Show (posixSecondsToUTCTime (fromInteger s)))
    safeHead (x : _) = Just x
    safeHead []      = Nothing
