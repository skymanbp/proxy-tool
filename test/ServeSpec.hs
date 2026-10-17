{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for @proxy-tool serve@, the standing proxy.
--
-- The pure decisions (which endpoint, the token estimate, the model-list
-- translation) are checked directly.  The standing proxy itself is checked
-- end to end on loopback: a stub OpenAI-compatible provider runs in-process
-- (warp's 'testWithApplication'), 'serveSession' runs in a thread against it,
-- and requests go through a real HTTP client.  No provider outside this
-- process is ever contacted, and no credential is involved -- the stub needs
-- none and the proxy token is a test constant.
module ServeSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, finally, try)
import Control.Monad (void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Client as H
import Network.HTTP.Types (mkStatus, status200, status404, statusCode)
import Network.Wai
  (Application, pathInfo, rawPathInfo, requestMethod, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Socket (close)
import System.Environment (setEnv, unsetEnv)
import System.Timeout (timeout)
import Test.Hspec

import ProxyTool.Config (defaultConfig)
import ProxyTool.I18n
import ProxyTool.ServeRoutes
import ProxyTool.Session (serveSession, stopServe, tokenFromEnv)
import ProxyTool.Types

spec :: Spec
spec = beforeAll_ (setEnv "PROXY_TOOL_SPEC_UPSTREAM_KEY" (T.unpack upstreamKey)) $ do
  routeSpec
  tokenSpec
  modelListSpec
  healthSpec
  envSpec
  liveSpec

-- ---------------------------------------------------------------------------
-- Pure decisions
-- ---------------------------------------------------------------------------

routeSpec :: Spec
routeSpec = describe "classifyServeRoute" $ do

  it "knows the four endpoints a standing proxy answers" $ do
    classifyServeRoute "GET"  ["healthz"]                        `shouldBe` ServeHealthz
    classifyServeRoute "POST" ["v1", "messages"]                 `shouldBe` ServeMessages
    classifyServeRoute "POST" ["v1", "messages", "count_tokens"] `shouldBe` ServeCountTokens
    classifyServeRoute "GET"  ["v1", "models"]                   `shouldBe` ServeModels
    classifyServeRoute "POST" ["admin", "shutdown"]              `shouldBe` ServeShutdown

  it "refuses the wrong method and any other path, instead of forwarding it" $ do
    classifyServeRoute "GET"  ["v1", "messages"]       `shouldBe` ServeNotFound
    classifyServeRoute "POST" ["healthz"]              `shouldBe` ServeNotFound
    classifyServeRoute "POST" ["v1", "complete"]       `shouldBe` ServeNotFound
    classifyServeRoute "GET"  ["v1", "models", "x"]    `shouldBe` ServeNotFound
    classifyServeRoute "GET"  ["admin", "shutdown"]    `shouldBe` ServeNotFound

tokenSpec :: Spec
tokenSpec = describe "estimateInputTokens" $ do

  it "counts system, messages and tools as compact JSON, four characters a token, rounded up" $ do
    -- {"messages":[],"system":"abcd","tools":[{"name":"t"}]} is 54 characters.
    estimateInputTokens (object [ "model" .= ("ignored" :: Text)
                                , "system" .= ("abcd" :: Text)
                                , "messages" .= ([] :: [Value])
                                , "tools" .= [object ["name" .= ("t" :: Text)]] ])
      `shouldBe` Right 14

  it "leaves out everything that is not prompt text" $
    -- {"messages":[{"content":"hi","role":"user"}]} is 45 characters.
    estimateInputTokens (object [ "model" .= ("m" :: Text), "max_tokens" .= (99 :: Int)
                                , "messages" .= [object [ "role" .= ("user" :: Text)
                                                        , "content" .= ("hi" :: Text) ]] ])
      `shouldBe` Right 12

  it "counts characters, not UTF-8 bytes" $
    -- {"system":"<two ideographs>"} is 15 characters but 19 bytes.
    estimateInputTokens (object ["system" .= ("\20320\22909" :: Text)]) `shouldBe` Right 4

  it "refuses a body that is not an object" $
    estimateInputTokens (Array V.empty) `shouldSatisfy` either (const True) (const False)

modelListSpec :: Spec
modelListSpec = describe "model listing" $ do

  it "translates an OpenAI list to Anthropic's shape" $ do
    let v = object [ "object" .= ("list" :: Text)
                   , "data" .= [ object ["id" .= ("a" :: Text), "created" .= (0 :: Int)]
                               , object ["id" .= ("b" :: Text)] ] ]
    case openAIModelsToAnthropic v of
      Left err -> expectationFailure (T.unpack err)
      Right out -> do
        ids out `shouldBe` ["a", "b"]
        field "first_id" out `shouldBe` Just (String "a")
        field "last_id" out  `shouldBe` Just (String "b")
        field "has_more" out `shouldBe` Just (Bool False)
        entries out !! 0 `shouldSatisfy` (\o -> KM.lookup "created_at" o == Just (String "1970-01-01T00:00:00Z"))
        entries out !! 1 `shouldSatisfy` (\o -> KM.lookup "created_at" o == Nothing)
        entries out !! 1 `shouldSatisfy` (\o -> KM.lookup "type" o == Just (String "model"))

  it "refuses a body with no data array" $
    openAIModelsToAnthropic (object []) `shouldSatisfy` either (const True) (const False)

  it "describes a provider without a list endpoint by its configured model alone" $ do
    ids (configuredModelList (Just "only-one")) `shouldBe` ["only-one"]
    ids (configuredModelList Nothing)           `shouldBe` []
    field "first_id" (configuredModelList Nothing) `shouldBe` Just Null

  it "treats only 404 and 405 as a missing list endpoint" $
    map modelListMissing [404, 405, 401, 429, 500] `shouldBe` [True, True, False, False, False]

healthSpec :: Spec
healthSpec = describe "healthzBody" $ do

  it "reports the loaded identity: model, effort, base URL, start time and pid" $ do
    let si = ServeInfo "venice" 8319 100 (Just "grok-4-20") (Just "high") "https://api.venice.ai/api/v1"
                       "2026-10-16T00:00:00Z" 4242
    field "model" (healthzBody si 101 Nothing)      `shouldBe` Just (String "grok-4-20")
    field "effort" (healthzBody si 101 Nothing)     `shouldBe` Just (String "high")
    field "base_url" (healthzBody si 101 Nothing)   `shouldBe` Just (String "https://api.venice.ai/api/v1")
    field "started_at" (healthzBody si 101 Nothing) `shouldBe` Just (String "2026-10-16T00:00:00Z")
    field "pid" (healthzBody si 101 Nothing)        `shouldBe` Just (Number 4242)
    let unforced = si { siModel = Nothing, siEffort = Nothing }
    field "model" (healthzBody unforced 101 Nothing)  `shouldBe` Just Null
    field "effort" (healthzBody unforced 101 Nothing) `shouldBe` Just Null

  it "compares tokens whole, byte by byte, without stopping at the first difference" $ do
    tokenMatches "secret-token" "secret-token" `shouldBe` True
    tokenMatches "secret-token" "secret-tokeX" `shouldBe` False
    tokenMatches "secret-token" "Xecret-token" `shouldBe` False
    tokenMatches "secret-token" "secret-toke"  `shouldBe` False
    tokenMatches "secret-token" "secret-token-" `shouldBe` False
    tokenMatches "secret-token" ""             `shouldBe` False
    tokenMatches "\35013\21345" "\35013\21345" `shouldBe` True

  it "reports provider, port and uptime, and token_ok only when a token was presented" $ do
    let si = ServeInfo "venice" 8319 100 Nothing Nothing "https://x/v1" "2026-10-16T00:00:00Z" 1
    field "ok" (healthzBody si 112.9 Nothing)       `shouldBe` Just (Bool True)
    field "uptime_s" (healthzBody si 112.9 Nothing) `shouldBe` Just (Number 12)
    field "port" (healthzBody si 112.9 Nothing)     `shouldBe` Just (Number 8319)
    field "token_ok" (healthzBody si 112.9 Nothing) `shouldBe` Nothing
    field "token_ok" (healthzBody si 112.9 (Just False)) `shouldBe` Just (Bool False)

envSpec :: Spec
envSpec = describe "tokenFromEnv" $ do

  it "reads the token from the named variable" $ do
    setEnv "PROXY_TOOL_SERVE_SPEC_TOKEN" "from-env"
    tokenFromEnv "PROXY_TOOL_SERVE_SPEC_TOKEN" `shouldReturn` Right "from-env"

  it "refuses an unset variable, naming the variable and not a value" $ do
    unsetEnv "PROXY_TOOL_SERVE_SPEC_TOKEN"
    tokenFromEnv "PROXY_TOOL_SERVE_SPEC_TOKEN"
      `shouldReturn` Left (MsgEnvVarNotSet "PROXY_TOOL_SERVE_SPEC_TOKEN")

-- ---------------------------------------------------------------------------
-- The standing proxy, end to end on loopback
-- ---------------------------------------------------------------------------

token :: Text
token = "serve-spec-token"

-- | A stand-in OpenAI-compatible provider.  It records every request it
-- receives, answers chat completions with a fixed reply, and answers
-- @GET \/v1\/models@ with whatever status the test set.
stubProvider :: IORef [(BS.ByteString, BS.ByteString)] -> IORef Int -> Application
stubProvider seen modelsStatus req respond = do
  atomicModifyIORef' seen (\xs -> (xs <> [(requestMethod req, rawPathInfo req)], ()))
  case (requestMethod req, pathInfo req) of
    ("POST", ["v1", "chat", "completions"]) ->
      respond $ responseLBS status200 json $ encode $ object
        [ "id" .= ("chatcmpl-1" :: Text), "object" .= ("chat.completion" :: Text)
        , "model" .= ("stub-model" :: Text)
        , "choices" .= [ object [ "index" .= (0 :: Int)
                                , "message" .= object [ "role" .= ("assistant" :: Text)
                                                      , "content" .= ("hello from stub" :: Text) ]
                                , "finish_reason" .= ("stop" :: Text) ] ]
        , "usage" .= object ["prompt_tokens" .= (3 :: Int), "completion_tokens" .= (4 :: Int)] ]
    ("GET", ["v1", "models"]) -> readIORef modelsStatus >>= \case
      200 -> respond $ responseLBS status200 json $ encode $ object
        [ "object" .= ("list" :: Text)
        , "data" .= [object ["id" .= ("m-one" :: Text)], object ["id" .= ("m-two" :: Text)]] ]
      s -> respond $ responseLBS (mkStatus s "stub") json
             "{\"error\":{\"message\":\"stub says no\"}}"
    _ -> respond (responseLBS status404 json "{}")
  where
    -- Connection: close, so the proxy's client does not keep an idle
    -- connection to the stub open: the stub's shutdown at the end of each
    -- test would otherwise wait out warp's 30 s idle timeout for it.
    json = [("content-type", "application/json"), ("connection", "close")]

stubFor :: Int -> Provider
stubFor upstreamPort = emptyProvider
  { provId      = "stub"
  , provWire    = WireOpenAIChat
  , provBaseUrl = "http://127.0.0.1:" <> T.pack (show upstreamPort) <> "/v1"
  , provKey     = SecretNone
  , provModel   = "stub-model"
  }

-- | Run 'serveSession' in a thread for the duration of @body@, which is
-- handed the port it bound.  Readiness and shutdown are both MVar handoffs:
-- the body starts only after the socket is listening, and this returns only
-- after the session has stopped the proxy.
withServe :: Provider -> Int -> (Int -> IO a) -> IO a
withServe provider port body = do
  ready <- newEmptyMVar
  stop  <- newEmptyMVar
  done  <- newEmptyMVar
  void $ forkIO $ do
    r <- serveSession (const (pure ())) defaultConfig provider port token
                      (putMVar ready . Right) (void (tryPutMVar stop ())) (takeMVar stop)
    case r of
      Left err -> void (tryPutMVar ready (Left err))
      Right () -> pure ()
    putMVar done r
  takeMVar ready >>= \case
    Left err -> do
      _ <- takeMVar done
      fail ("serve refused to start: " <> T.unpack (render LangEn err))
    Right p -> body p `finally` (putMVar stop () >> void (takeMVar done))

withStub :: (IORef [(BS.ByteString, BS.ByteString)] -> IORef Int -> Int -> IO a) -> IO a
withStub k = do
  seen <- newIORef []
  modelsStatus <- newIORef 200
  Warp.testWithApplication (pure (stubProvider seen modelsStatus)) (k seen modelsStatus)

call :: H.Manager -> Int -> BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)]
     -> BL.ByteString -> IO (H.Response BL.ByteString)
call mgr port method path hdrs body = do
  base <- H.parseRequest ("http://127.0.0.1:" <> show port)
  let (p, q) = BS.break (== 63) path   -- 63 = '?'
  H.httpLbs base { H.method = method, H.path = p, H.queryString = q
                 , H.requestHeaders = [ (CI.mk k, v) | (k, v) <- hdrs ]
                 , H.requestBody = H.RequestBodyLBS body } mgr

bearer :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
bearer t = [("authorization", "Bearer " <> t), ("content-type", "application/json")]

messagesBody :: BL.ByteString
messagesBody = encode $ object
  [ "model" .= ("claude-whatever" :: Text), "max_tokens" .= (16 :: Int)
  , "messages" .= [object ["role" .= ("user" :: Text), "content" .= ("hi" :: Text)]] ]

liveSpec :: Spec
liveSpec = describe "serveSession (stub provider on loopback)" $ do

  it "answers /healthz without a token, and says whether a presented token is right" $
    withStub $ \_ _ up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      anon <- call mgr port "GET" "/healthz" [] ""
      statusCode (H.responseStatus anon) `shouldBe` 200
      let body = decode (H.responseBody anon) :: Maybe Value
      (body >>= field "provider") `shouldBe` Just (String "stub")
      (body >>= field "port") `shouldBe` Just (Number (fromIntegral port))
      (body >>= field "token_ok") `shouldBe` Nothing
      right <- call mgr port "GET" "/healthz" (bearer (enc token)) ""
      (decode (H.responseBody right) >>= field "token_ok") `shouldBe` Just (Bool True)
      wrong <- call mgr port "GET" "/healthz" (bearer "nope") ""
      (decode (H.responseBody wrong) >>= field "token_ok") `shouldBe` Just (Bool False)

  it "refuses a missing or wrong token with 401 and forwards nothing" $
    withStub $ \seen _ up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      none <- call mgr port "POST" "/v1/messages" [("content-type", "application/json")] messagesBody
      statusCode (H.responseStatus none) `shouldBe` 401
      bad <- call mgr port "POST" "/v1/messages" [("x-api-key", "nope")] messagesBody
      statusCode (H.responseStatus bad) `shouldBe` 401
      readIORef seen `shouldReturn` []

  it "translates POST /v1/messages to the provider's chat endpoint and back" $
    withStub $ \seen _ up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      r <- call mgr port "POST" "/v1/messages?beta=true" (bearer (enc token)) messagesBody
      statusCode (H.responseStatus r) `shouldBe` 200
      let body = decode (H.responseBody r) :: Maybe Value
      (body >>= field "type") `shouldBe` Just (String "message")
      BL.toStrict (H.responseBody r) `shouldSatisfy` BS.isInfixOf "hello from stub"
      map fst <$> readIORef seen `shouldReturn` ["POST"]
      map snd <$> readIORef seen `shouldReturn` ["/v1/chat/completions"]

  it "answers count_tokens locally, without asking the provider" $
    withStub $ \seen _ up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      r <- call mgr port "POST" "/v1/messages/count_tokens" (bearer (enc token)) messagesBody
      statusCode (H.responseStatus r) `shouldBe` 200
      (decode (H.responseBody r) >>= field "input_tokens") `shouldBe` Just (Number 12)
      readIORef seen `shouldReturn` []

  it "lists the provider's models in Anthropic's shape" $
    withStub $ \_ _ up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      r <- call mgr port "GET" "/v1/models" (bearer (enc token)) ""
      statusCode (H.responseStatus r) `shouldBe` 200
      (ids <$> decode (H.responseBody r)) `shouldBe` Just ["m-one", "m-two"]

  it "falls back to the configured model only when the provider has no list endpoint" $
    withStub $ \_ status up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      writeIORef status 404
      r <- call mgr port "GET" "/v1/models" (bearer (enc token)) ""
      statusCode (H.responseStatus r) `shouldBe` 200
      (ids <$> decode (H.responseBody r)) `shouldBe` Just ["stub-model"]

  it "passes any other provider error through with its status and body" $
    withStub $ \_ status up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      writeIORef status 429
      r <- call mgr port "GET" "/v1/models" (bearer (enc token)) ""
      statusCode (H.responseStatus r) `shouldBe` 429
      BL.toStrict (H.responseBody r) `shouldSatisfy` BS.isInfixOf "stub says no"

  it "answers 404 for an endpoint it does not serve" $
    withStub $ \seen _ up -> withServe (stubFor up) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      r <- call mgr port "POST" "/v1/complete" (bearer (enc token)) messagesBody
      statusCode (H.responseStatus r) `shouldBe` 404
      readIORef seen `shouldReturn` []

  it "answers 502 when the provider cannot be reached, with no retry and no substitute" $ do
    (deadPort, sock) <- Warp.openFreePort
    close sock
    withServe (stubFor deadPort) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      m <- call mgr port "POST" "/v1/messages" (bearer (enc token)) messagesBody
      statusCode (H.responseStatus m) `shouldBe` 502
      l <- call mgr port "GET" "/v1/models" (bearer (enc token)) ""
      statusCode (H.responseStatus l) `shouldBe` 502

  it "refuses a second proxy on a port that is already serving" $
    withStub $ \_ _ up -> withServe (stubFor up) 0 $ \port -> do
      second <- serveSession (const (pure ())) defaultConfig (stubFor up) port token
                             (const (pure ())) (pure ()) (pure ())
      second `shouldSatisfy` \case
        Left (MsgProxyBindFailed _) -> True
        _                           -> False

  it "stops listening once it is told to stop" $ do
    portVar <- newEmptyMVar
    withStub $ \_ _ up -> withServe (stubFor up) 0 (putMVar portVar)
    port <- takeMVar portVar
    mgr <- H.newManager H.defaultManagerSettings
    r <- try (call mgr port "GET" "/healthz" [] "")
    case r of
      Left (_ :: SomeException) -> pure ()
      Right res -> expectationFailure ("still answering after stop: "
                                       <> show (statusCode (H.responseStatus res)))

  it "refuses a provider with no base URL before binding anything" $ do
    r <- serveSession (const (pure ())) defaultConfig emptyProvider { provId = "nobase" } 0 token
                      (const (pure ())) (pure ()) (pure ())
    r `shouldBe` Left (MsgProviderNoBaseUrl "nobase")

  it "reports the identity it loaded on /healthz, and never a credential" $
    withStub $ \_ _ up -> withServe (stubFor up) { provKey = SecretEnv "PROXY_TOOL_SPEC_UPSTREAM_KEY" } 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      r <- call mgr port "GET" "/healthz" (bearer (enc token)) ""
      let body = decode (H.responseBody r) :: Maybe Value
      (body >>= field "model") `shouldBe` Just (String "stub-model")
      (body >>= field "effort") `shouldBe` Just Null
      (body >>= field "base_url") `shouldBe` Just (String ("http://127.0.0.1:" <> T.pack (show up) <> "/v1"))
      (body >>= field "pid") `shouldSatisfy` \case
        Just (Number n) -> n > 0
        _               -> False
      (body >>= field "started_at") `shouldSatisfy` \case
        Just (String s) -> "T" `T.isInfixOf` s
        _               -> False
      BL.toStrict (H.responseBody r) `shouldNotSatisfy` BS.isInfixOf (enc token)
      BL.toStrict (H.responseBody r) `shouldNotSatisfy` BS.isInfixOf (enc upstreamKey)

  it "stops on an authorised POST /admin/shutdown and refuses a wrong token with 401" $
    withStub $ \_ _ up -> do
      (port, done) <- startServe (stubFor up)
      mgr <- H.newManager H.defaultManagerSettings
      bad <- call mgr port "POST" "/admin/shutdown" (bearer "nope") ""
      statusCode (H.responseStatus bad) `shouldBe` 401
      still <- call mgr port "GET" "/healthz" [] ""
      statusCode (H.responseStatus still) `shouldBe` 200
      none <- call mgr port "POST" "/admin/shutdown" [] ""
      statusCode (H.responseStatus none) `shouldBe` 401
      ok <- call mgr port "POST" "/admin/shutdown" (bearer (enc token)) ""
      statusCode (H.responseStatus ok) `shouldBe` 200
      timeout 5000000 (takeMVar done) `shouldReturn` Just (Right ())

  it "serve-stop refuses a wrong token, stops with the right one, then finds nothing to stop" $
    withStub $ \_ _ up -> do
      (port, done) <- startServe (stubFor up)
      stopServe port "nope" `shouldReturn` Left (MsgServeStopRefused port)
      stopServe port token `shouldReturn` Right ()
      timeout 5000000 (takeMVar done) `shouldReturn` Just (Right ())
      stopServe port token `shouldReturn` Left (MsgServeStopUnreachable port)

  it "never reflects the provider key or the proxy token in a 502 or passed-through error body" $ do
    (deadPort, sock) <- Warp.openFreePort
    close sock
    let keyed p = p { provKey = SecretEnv "PROXY_TOOL_SPEC_UPSTREAM_KEY" }
        clean r = do
          BL.toStrict (H.responseBody r) `shouldNotSatisfy` BS.isInfixOf (enc upstreamKey)
          BL.toStrict (H.responseBody r) `shouldNotSatisfy` BS.isInfixOf (enc token)
    withServe (keyed (stubFor deadPort)) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      m <- call mgr port "POST" "/v1/messages" (bearer (enc token)) messagesBody
      statusCode (H.responseStatus m) `shouldBe` 502
      clean m
      l <- call mgr port "GET" "/v1/models" (bearer (enc token)) ""
      statusCode (H.responseStatus l) `shouldBe` 502
      clean l
    withStub $ \_ status up -> withServe (keyed (stubFor up)) 0 $ \port -> do
      mgr <- H.newManager H.defaultManagerSettings
      writeIORef status 502
      r <- call mgr port "GET" "/v1/models" (bearer (enc token)) ""
      statusCode (H.responseStatus r) `shouldBe` 502
      clean r

-- | The upstream key the leak rows run with (set in the environment the
-- provider entry names; the stub provider ignores it).
upstreamKey :: Text
upstreamKey = "upstream-key-fixture-0123"

-- | Start a standing proxy whose only way to stop is its own
-- @POST \/admin\/shutdown@ (no test-side stop): the port, and the MVar
-- 'serveSession' fills when it returns.
startServe :: Provider -> IO (Int, MVar (Either Msg ()))
startServe provider = do
  ready <- newEmptyMVar
  stop  <- newEmptyMVar
  done  <- newEmptyMVar
  void $ forkIO $ do
    r <- serveSession (const (pure ())) defaultConfig provider 0 token
                      (putMVar ready) (void (tryPutMVar stop ())) (takeMVar stop)
    putMVar done r
  timeout 5000000 (takeMVar ready) >>= \case
    Just p  -> pure (p, done)
    Nothing -> fail "serve did not start within 5 s"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

enc :: Text -> BS.ByteString
enc = BS.pack . map (fromIntegral . fromEnum) . T.unpack

field :: Key -> Value -> Maybe Value
field k = \case
  Object o -> KM.lookup k o
  _        -> Nothing

entries :: Value -> [Object]
entries v = case field "data" v of
  Just (Array xs) -> [o | Object o <- V.toList xs]
  _               -> []

ids :: Value -> [Text]
ids v = [t | o <- entries v, Just (String t) <- [KM.lookup "id" o]]
