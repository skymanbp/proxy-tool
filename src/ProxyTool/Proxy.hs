{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The local HTTP proxy that sits between a CLI agent and a provider.
--
-- It exists to do three things an agent cannot be told to do by itself: force
-- the model regardless of what the agent asked for, inject a reasoning effort
-- the agent has no flag for, and -- when the two ends speak different
-- dialects -- translate the request and the response, including the streaming
-- case.
--
-- It listens on loopback only and requires a per-session bearer token, so it
-- is not an open relay to the user's paid API for every other process on the
-- machine.
module ProxyTool.Proxy
  ( ProxyConfig (..)
  , ProxyHandle (..)
  , startProxy
  , stopProxy
  , proxyApplication
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (when)
import Data.Aeson (Value (Bool, Object), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Header, Status, status401, status502, statusCode)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType)
  , SockAddr (SockAddrInet, SockAddrInet6)
  , Socket
  , SocketOption (ReuseAddr)
  , SocketType (Stream)
  , bind
  , close
  , defaultHints
  , getAddrInfo
  , getSocketName
  , listen
  , setSocketOption
  , socket
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , ResponseReceived
  , rawPathInfo
  , rawQueryString
  , requestHeaders
  , requestMethod
  , responseLBS
  , responseStream
  , strictRequestBody
  )
import qualified Network.Wai.Handler.Warp as Warp

import ProxyTool.I18n
import ProxyTool.Translate
import ProxyTool.Types

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data ProxyConfig = ProxyConfig
  { pcUpstreamBase :: Text
    -- ^ Provider base URL, e.g. @https:\/\/api.openai.com\/v1@.
  , pcUpstreamWire :: Wire
  , pcClientWire   :: Wire
    -- ^ Dialect the agent speaks.  When it differs from 'pcUpstreamWire' the
    -- proxy translates instead of merely rewriting.
  , pcApiKey       :: Text
    -- ^ Credential presented upstream.  Never logged.
  , pcHeaders      :: [(Text, Text)]
  , pcOpts         :: TranslateOpts
  , pcToken        :: Text
    -- ^ Token the agent must present.  Empty disables the check.
  , pcLog          :: Text -> IO ()
  , pcLogBodies    :: Bool
  , pcLang         :: Lang
    -- ^ Language for the diagnostics this proxy writes to the terminal.  The
    -- JSON error bodies it returns to the agent stay English: those are read
    -- by a program, not by a person.
  }

data ProxyHandle = ProxyHandle
  { phPort   :: Int
  , phSocket :: Socket
  , phThread :: ThreadId
  }

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Bind, then serve in a background thread.
--
-- The socket is bound before this returns, so the caller learns the real port
-- before it launches the agent.  Asking the OS for a free port (passing
-- 'Nothing') is the default because it lets several sessions run side by
-- side; reading the port back off an already-started server would be a race.
startProxy :: ProxyConfig -> Maybe Int -> IO (Either Msg ProxyHandle)
startProxy pc mport =
  try (bindLoopback (fromMaybe 0 mport)) >>= \case
    Left (e :: SomeException) ->
      pure (Left (MsgProxyBindFailed (T.pack (show e))))
    Right (sock, port) ->
      try (H.newManager tlsManagerSettings
             { H.managerResponseTimeout = H.responseTimeoutNone }) >>= \case
        Left (e :: SomeException) -> do
          close sock
          pure (Left (MsgProxyManagerFailed (T.pack (show e))))
        Right mgr -> do
          let settings = Warp.setOnException
                (\_ e -> pcLog pc ("proxy: " <> T.pack (show e)))
                Warp.defaultSettings
          tid <- forkIO (Warp.runSettingsSocket settings sock
                           (proxyApplication mgr pc))
          pure (Right (ProxyHandle port sock tid))

-- | Bind a listening socket on loopback, returning the port actually taken.
--
-- The address comes from 'getAddrInfo' rather than a hand-built literal, so
-- this still works on a host that resolves loopback to IPv6.
bindLoopback :: Int -> IO (Socket, Int)
bindLoopback wanted = do
  addrs <- getAddrInfo (Just defaultHints { addrSocketType = Stream })
                       (Just "127.0.0.1")
                       (Just (show wanted))
  case addrs of
    [] -> fail "no address for 127.0.0.1"
    (ai : _) -> bracketOnError
      (socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai))
      close
      $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress ai)
        listen sock 128
        name <- getSocketName sock
        port <- case name of
          SockAddrInet p _      -> pure (fromIntegral p)
          SockAddrInet6 p _ _ _ -> pure (fromIntegral p)
          _                     -> fail "listening socket has no port"
        pure (sock, port)

stopProxy :: ProxyHandle -> IO ()
stopProxy h = do
  killThread (phThread h)
  close (phSocket h)

-- ---------------------------------------------------------------------------
-- The application
-- ---------------------------------------------------------------------------

-- | What the proxy decided to do with one request.
data RequestPlan = RequestPlan
  { planTranslate :: Bool
  , planStreaming :: Bool
  , planModel     :: Text
  , planBody      :: BL.ByteString
  }

proxyApplication :: H.Manager -> ProxyConfig -> Application
proxyApplication mgr pc req respond
  | not (authorised pc req) =
      respond $ errorResponse pc status401 "authentication_error"
        "missing or incorrect proxy token"
  | otherwise = do
      body <- strictRequestBody req
      case buildUpstream pc req body of
        Left err -> respond $ errorResponse pc status502 "invalid_request_error" err
        Right (upstreamReq, plan) -> do
          when (pcLogBodies pc) $
            pcLog pc $ "-> " <> TE.decodeUtf8 (requestMethod req) <> " "
                    <> TE.decodeUtf8 (H.path upstreamReq) <> " "
                    <> TE.decodeUtf8 (BL.toStrict (BL.take 4096 (planBody plan)))
          try (forward mgr pc upstreamReq plan respond) >>= \case
            Right rr -> pure rr
            Left (e :: SomeException) -> do
              pcLog pc (render (pcLang pc) (MsgUpstreamFailed (T.pack (show e))))
              respond $ errorResponse pc status502 "api_error"
                ("upstream request failed: " <> T.pack (show e))

-- ---------------------------------------------------------------------------
-- Authorisation
-- ---------------------------------------------------------------------------

-- | Accept the session token from either dialect's credential header.
--
-- Header names from wai are already case-insensitive, so the literal patterns
-- below match regardless of how the agent capitalised them.
authorised :: ProxyConfig -> Request -> Bool
authorised pc req
  | T.null (pcToken pc) = True
  | otherwise           = pcToken pc `elem` presented
  where
    hdrs = requestHeaders req
    presented =
      [ stripBearer (TE.decodeUtf8 v) | ("authorization", v) <- hdrs ]
        <> [ T.strip (TE.decodeUtf8 v) | ("x-api-key", v) <- hdrs ]
    stripBearer t =
      let s = T.strip t
      in if "Bearer " `T.isPrefixOf` s then T.strip (T.drop 7 s) else s

-- ---------------------------------------------------------------------------
-- Building the upstream request
-- ---------------------------------------------------------------------------

buildUpstream
  :: ProxyConfig
  -> Request
  -> BL.ByteString
  -> Either Text (H.Request, RequestPlan)
buildUpstream pc req rawBody = do
  base <- case H.parseRequest (T.unpack (T.strip (pcUpstreamBase pc))) of
    Nothing -> Left ("provider base URL is not a valid URL: " <> pcUpstreamBase pc)
    Just r  -> Right r
  let translating = pcClientWire pc /= pcUpstreamWire pc
      incoming    = decode rawBody :: Maybe Value
      streaming   = maybe False bodyWantsStream incoming
  (newBody, suffix) <-
    if translating
      then do
        v <- maybe (Left "request body was not JSON, so it cannot be translated")
                   Right incoming
        translated <- anthropicToOpenAIRequest (pcOpts pc) v
        -- Translating implies we know the target endpoint, so the incoming
        -- path is replaced rather than appended to the base path.
        Right (encode translated, "/chat/completions")
      else
        let rewritten = case incoming of
              Nothing -> rawBody
              Just v  -> encode $ case pcUpstreamWire pc of
                WireAnthropic  -> rewriteAnthropicRequest (pcOpts pc) v
                WireOpenAIChat -> rewriteOpenAIRequest (pcOpts pc) v
        in Right (rewritten, rawPathInfo req)
  let upstream = base
        { H.method          = requestMethod req
        , H.path            = stripTrailingSlashBS (H.path base) <> suffix
        , H.queryString     = rawQueryString req
        , H.requestHeaders  = upstreamHeaders pc req
        , H.requestBody     = H.RequestBodyLBS newBody
        , H.responseTimeout = H.responseTimeoutNone
          -- A long generation must not be cut off by the client-side default.
        , H.redirectCount   = 0
          -- Following a redirect would re-send the injected credential to a
          -- host the user never configured.
        , H.decompress      = const False
          -- Pass bytes through untouched; 'accept-encoding' is dropped below,
          -- so upstream answers with identity anyway.
        }
  pure ( upstream
       , RequestPlan
           { planTranslate = translating
           , planStreaming = streaming
           , planModel     = fromMaybe "" (optModel (pcOpts pc))
           , planBody      = newBody
           } )

bodyWantsStream :: Value -> Bool
bodyWantsStream (Object o) = KM.lookup "stream" o == Just (Bool True)
bodyWantsStream _          = False

stripTrailingSlashBS :: ByteString -> ByteString
stripTrailingSlashBS b
  | not (BS.null b) && BS.last b == 47 = stripTrailingSlashBS (BS.init b)
  | otherwise                          = b

-- | Headers for the upstream request.
--
-- The client's own credential is dropped -- it presented the session token,
-- not the provider's key -- and replaced with the credential shape the
-- upstream dialect expects.
upstreamHeaders :: ProxyConfig -> Request -> [Header]
upstreamHeaders pc req =
  kept <> auth <> extra <> [identityEncoding]
  where
    -- http-client inserts "Accept-Encoding: gzip" whenever the header is
    -- absent, and keeps any explicit value instead (http-client-0.7.19,
    -- Network/HTTP/Client/Request.hs:533).  Since this proxy disables
    -- decompression and strips content-encoding from the response, a gzipping
    -- upstream would otherwise hand the agent compressed bytes labelled as
    -- plain text.  Asking for identity keeps the body byte-for-byte forwardable.
    identityEncoding = ("accept-encoding", "identity")

    -- Content-Length is deliberately NOT set here.  http-client prepends its
    -- own, computed from the request body, without checking whether one is
    -- already present (same file, contentLengthHeader).  Setting it here too
    -- put two Content-Length fields on the wire, which RFC 9110 section 8.6
    -- allows a recipient to reject outright as request smuggling.
    kept = [ (k, v) | (k, v) <- requestHeaders req
                    , CI.foldedCase k `notElem` dropped ]
    dropped =
      [ "host", "connection", "keep-alive", "proxy-authenticate"
      , "proxy-authorization", "te", "trailer", "transfer-encoding"
      , "upgrade", "content-length", "accept-encoding"
      , "authorization", "x-api-key"
      ]
    key = pcApiKey pc
    auth
      | T.null key = []
      | otherwise = case pcUpstreamWire pc of
          WireAnthropic  -> [("x-api-key", TE.encodeUtf8 key)]
          WireOpenAIChat -> [("authorization", "Bearer " <> TE.encodeUtf8 key)]
    extra = [ (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- pcHeaders pc ]

-- ---------------------------------------------------------------------------
-- Forwarding
-- ---------------------------------------------------------------------------

forward
  :: H.Manager
  -> ProxyConfig
  -> H.Request
  -> RequestPlan
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
forward mgr pc upstream plan respond =
  H.withResponse upstream mgr $ \res -> do
    let status = H.responseStatus res
        hdrs   = responseHeadersFor res
        reader = H.responseBody res
    if not (planTranslate plan)
      then respond $ responseStream status hdrs (pumpRaw reader)
      else if planStreaming plan && statusCode status < 400
        then respond $ responseStream status (sseHeaders hdrs)
               (pumpTranslatedStream pc plan reader)
        else do
          full <- BL.fromChunks <$> H.brConsume reader
          let translated = case decode full of
                Nothing -> encode $ errorBody "api_error" $
                  "upstream returned a non-JSON body: "
                    <> T.take 400 (TE.decodeUtf8 (BL.toStrict full))
                Just v -> case openAIToAnthropicResponse v of
                  Right out -> encode out
                  Left err  -> encode $ errorBody "api_error"
                    ("could not translate upstream response: " <> err)
          respond $ responseLBS status (jsonHeaders hdrs) translated

-- | Copy bytes through unchanged, flushing every chunk so a server-sent event
-- stream is not held back by buffering.
pumpRaw :: H.BodyReader -> (Builder -> IO ()) -> IO () -> IO ()
pumpRaw reader write flush = loop
  where
    loop = do
      chunk <- H.brRead reader
      if BS.null chunk
        then flush
        else write (byteString chunk) >> flush >> loop

-- | Largest partial event line held while waiting for its terminating
-- newline.  Generous next to a real completion chunk; small next to the
-- unbounded growth a badly framed upstream would otherwise cause.
maxPartialLine :: Int
maxPartialLine = 16 * 1024 * 1024

-- | Consume an OpenAI event stream and emit an Anthropic one.
--
-- The leftover buffer matters: a chunk boundary can fall in the middle of an
-- SSE line, and half a JSON payload must not be handed to the parser.
pumpTranslatedStream
  :: ProxyConfig
  -> RequestPlan
  -> H.BodyReader
  -> (Builder -> IO ())
  -> IO ()
  -> IO ()
pumpTranslatedStream pc plan reader write flush = do
  stateRef <- newIORef (newStreamState (planModel plan))
  bufRef   <- newIORef BS.empty
  let emit evs = do
        mapM_ (write . byteString . renderSseEvent) evs
        flush
      stepOne (s, evs, ws) l =
        let (s', e, w) = stepStream s l
        in (s', evs <> e, ws <> w)
      loop = do
        chunk <- H.brRead reader
        if BS.null chunk
          then do
            st <- readIORef stateRef
            let (st', evs) = finishStream st
            writeIORef stateRef st'
            emit evs
          else do
            leftover <- readIORef bufRef
            let (leftover0, ls) = splitSseLines leftover chunk
            -- An upstream that never emits a newline would otherwise grow this
            -- buffer for as long as it keeps sending.  A real event line is
            -- kilobytes; past the cap the stream is malformed, so the partial
            -- line is dropped and said so, rather than consuming memory until
            -- the process dies.
            leftover' <- if BS.length leftover0 > maxPartialLine
              then do
                pcLog pc $ "stream: dropping a partial event line over "
                        <> T.pack (show maxPartialLine)
                        <> " bytes; upstream is not framing events"
                pure BS.empty
              else pure leftover0
            writeIORef bufRef leftover'
            st <- readIORef stateRef
            let (st', evs, warns) = foldl stepOne (st, [], []) ls
            writeIORef stateRef st'
            mapM_ (\w -> pcLog pc ("stream: " <> w)) warns
            emit evs
            loop
  loop

-- ---------------------------------------------------------------------------
-- Response headers
-- ---------------------------------------------------------------------------

-- | Upstream response headers, minus the ones describing a framing we are
-- about to change.
responseHeadersFor :: H.Response H.BodyReader -> [Header]
responseHeadersFor res =
  [ (k, v)
  | (k, v) <- H.responseHeaders res
  , CI.foldedCase k `notElem`
      [ "content-length", "transfer-encoding", "content-encoding"
      , "connection", "keep-alive", "trailer", "upgrade"
      ]
  ]

sseHeaders :: [Header] -> [Header]
sseHeaders hdrs =
  ("content-type", "text/event-stream; charset=utf-8")
    : ("cache-control", "no-cache")
    : [h | h@(k, _) <- hdrs, CI.foldedCase k /= "content-type"]

jsonHeaders :: [Header] -> [Header]
jsonHeaders hdrs =
  ("content-type", "application/json")
    : [h | h@(k, _) <- hdrs, CI.foldedCase k /= "content-type"]

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Report a proxy-level failure in whichever dialect the agent understands,
-- so it surfaces in the agent's UI instead of as a parse failure.
errorResponse :: ProxyConfig -> Status -> Text -> Text -> Response
errorResponse pc status kind msg =
  responseLBS status [("content-type", "application/json")] $
    encode $ case pcClientWire pc of
      WireAnthropic  -> errorBody kind msg
      WireOpenAIChat -> object
        [ "error" .= object ["type" .= kind, "message" .= msg] ]

errorBody :: Text -> Text -> Value
errorBody kind msg = object
  [ "type"  .= ("error" :: Text)
  , "error" .= object ["type" .= kind, "message" .= msg]
  ]
