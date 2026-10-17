{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command line front end.
--
-- With no arguments this is the TUI.  The subcommands exist so the same
-- machinery can be driven from a script, and so that what a launch would do
-- can be inspected without launching anything.
--
-- Language resolution happens once, here, and is then passed down: the
-- library returns 'Msg' values and this module decides how to say them.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception
  ( AsyncException (UserInterrupt)
  , IOException
  , SomeException
  , bracket
  , handle
  , throwIO
  , try
  )
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO
  ( BufferMode (LineBuffering)
  , hSetBinaryMode
  , hSetBuffering
  , hSetEncoding
  , stderr
  , stdin
  , stdout
  , utf8
  )

#if defined(mingw32_HOST_OS)
import qualified GHC.ConsoleHandler as Console
import System.Win32.Console (getConsoleOutputCP, setConsoleOutputCP)
import System.Win32.Types (UINT)
#else
import qualified System.Posix.Signals as Signals
#endif

import ProxyTool.Config
import ProxyTool.I18n
import ProxyTool.Launch
  ( CommandResolution (crVia)
  , LaunchPlan (lpRoute)
  , PreviewLine (plText)
  , planLaunch
  , resolveCommand
  , resolveSecret
  , routeSummary
  )
import ProxyTool.Session
import ProxyTool.TUI (runTui)
import ProxyTool.Types

main :: IO ()
main = withUnicodeOutput $ do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  argv <- getArgs
  envLang <- (>>= parseLang . T.pack) <$> lookupEnv "PROXY_TOOL_LANG"
  case takeLangFlag argv of
    -- The config has not been read yet, so its language is not yet knowable;
    -- a bootstrap failure is reported in whatever was asked for explicitly,
    -- and in English otherwise.
    Left err -> dieWithUsage (firstOf [envLang] LangEn) err
    Right (flagLang, args) -> loadConfig >>= \case
      Left err -> die (firstOf [flagLang, envLang] LangEn) err
      Right (path, cfg0) -> do
        let lang = firstOf [flagLang, envLang] (cfgLang cfg0)
            -- @--lang@ is an explicit choice and is remembered; the
            -- environment variable is scoped to this run and is not.
            cfg = maybe cfg0 (\l -> cfg0 { cfgLang = l }) flagLang
        run lang path cfg args

-- | Make the standard handles able to carry every language this tool speaks.
--
-- GHC encodes the standard handles with the machine's locale encoding, which
-- on Windows is the ANSI\/console code page.  That is not a display problem
-- but a hard failure: asking a CP1252 machine to print Chinese throws
-- @commitAndReleaseBuffer: invalid argument (cannot encode character)@ and
-- the command produces no output at all.  So the handles are set to UTF-8,
-- and on Windows the console is switched to the UTF-8 code page for the
-- lifetime of the process so those bytes are also /rendered/ correctly --
-- then switched back, because the code page outlives this process and
-- belongs to the user's shell, not to us.
--
-- A machine with no console attached has no code page to change; that is not
-- an error, and the UTF-8 handles still do the right thing when the output is
-- redirected to a file.
withUnicodeOutput :: IO a -> IO a
withUnicodeOutput act = withConsoleCodePage $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  act

withConsoleCodePage :: IO a -> IO a
#if defined(mingw32_HOST_OS)
withConsoleCodePage act = bracket acquire restore (const act)
  where
    utf8CodePage = 65001 :: UINT

    acquire :: IO (Either SomeException UINT)
    acquire = try $ do
      before <- getConsoleOutputCP
      setConsoleOutputCP utf8CodePage
      pure before

    restore :: Either SomeException UINT -> IO ()
    restore = \case
      Right before | before /= utf8CodePage ->
        void (try (setConsoleOutputCP before) :: IO (Either SomeException ()))
      _ -> pure ()
#else
withConsoleCodePage = id
#endif

run :: Lang -> FilePath -> Config -> [String] -> IO ()
run lang path cfg = \case
  []                 -> tui lang path cfg
  ["tui"]            -> tui lang path cfg
  ["list"]           -> listAll lang cfg
  ["doctor"]         -> doctor lang cfg path
  ["config", "path"] -> TIO.putStrLn (T.pack path)
  ["config", "show"] -> BLC.putStrLn (encodeConfig cfg)
  ["config", "init"] -> initConfig lang path cfg
  ("plan" : rest)    -> withPair lang cfg rest (planCmd lang)
  ("run"  : rest)    -> withPair lang cfg rest (runCmd lang)
  ("serve" : rest)   -> serveCmd lang cfg rest
  ("serve-stop" : rest) -> serveStopCmd lang rest
  ["--help"]         -> TIO.putStrLn (usageText lang)
  ["-h"]             -> TIO.putStrLn (usageText lang)
  ["--version"]      -> TIO.putStrLn "proxy-tool 0.1.0.0"
  args               -> dieWithUsage lang (MsgUnrecognisedArgs
                                             (T.unwords (map T.pack args)))

-- | Pull @--lang@ out of the argument list before dispatch, so it can be
-- written anywhere on the line and every subcommand honours it.
takeLangFlag :: [String] -> Either Msg (Maybe Lang, [String])
takeLangFlag = go Nothing
  where
    go acc [] = Right (acc, [])
    go _   ["--lang"] = Left MsgLangNeedsValue
    go _   ("--lang" : v : _) | Nothing <- parseLang (T.pack v) =
      Left (MsgUnknownLang (T.pack v))
    go _   ("--lang" : v : xs) = go (parseLang (T.pack v)) xs
    go acc (x : xs) = do
      (l, rest) <- go acc xs
      pure (l, x : rest)

-- | The first language anyone actually asked for, or the fallback.
firstOf :: [Maybe a] -> a -> a
firstOf xs fallback = case [x | Just x <- xs] of
  (x : _) -> x
  []      -> fallback

die :: Lang -> Msg -> IO ()
die lang msg = do
  TIO.hPutStrLn stderr (render lang msg)
  exitWith (ExitFailure 2)

-- | Usage is worth repeating when the command line itself was wrong, and
-- noise otherwise.
dieWithUsage :: Lang -> Msg -> IO ()
dieWithUsage lang msg = do
  TIO.hPutStrLn stderr (render lang msg)
  TIO.hPutStrLn stderr ""
  TIO.hPutStrLn stderr (usageText lang)
  exitWith (ExitFailure 2)

-- ---------------------------------------------------------------------------
-- Subcommands
-- ---------------------------------------------------------------------------

tui :: Lang -> FilePath -> Config -> IO ()
tui lang path cfg = do
  cfg' <- runTui path lang cfg
  saveConfig path cfg' >>= \case
    Left err -> die lang err
    Right () -> pure ()

initConfig :: Lang -> FilePath -> Config -> IO ()
initConfig lang path cfg = saveConfig path cfg >>= \case
  Left err -> die lang err
  Right () -> TIO.putStrLn (render lang (MsgWroteFile (T.pack path)))

listAll :: Lang -> Config -> IO ()
listAll lang cfg = do
  TIO.putStrLn (say MsgCliAgents)
  mapM_ (\a -> TIO.putStrLn $ "  " <> pad 10 (agId a) <> pad 22 (agLabel a)
                           <> pad 26 (wireLabel (agWire a)) <> agCommand a)
        (cfgAgents cfg)
  TIO.putStrLn ""
  TIO.putStrLn (say MsgCliProviders)
  mapM_ (\p -> TIO.putStrLn $ "  " <> pad 22 (provId p)
                           <> pad 26 (wireLabel (provWire p))
                           <> pad 34 (provBaseUrl p) <> provModel p)
        (cfgProviders cfg)
  TIO.putStrLn ""
  TIO.putStrLn (say (MsgCliRoute (routeLabel (cfgRoute cfg))))
  where say = render lang

-- | Pad to a column count, not a character count: an agent labelled in
-- Chinese is half as many characters as it is columns wide.
pad :: Int -> Text -> Text
pad n t = padWidth (max (textWidth t + 1) n) t

-- | Resolve the agent/provider pair named on the command line, applying an
-- optional @--route@ override.
withPair
  :: Lang
  -> Config
  -> [String]
  -> (Agent -> Provider -> Config -> IO ())
  -> IO ()
withPair lang cfg rest k = case parseArgs rest of
  Left err -> dieWithUsage lang err
  Right (aid, pid, mroute) ->
    case (findAgent aid cfg, findProvider pid cfg) of
      (Nothing, _)     -> die lang (MsgNoSuchAgent aid)
      (_, Nothing)     -> die lang (MsgNoSuchProvider pid)
      (Just a, Just p) ->
        k a p cfg { cfgRoute = maybe (cfgRoute cfg) id mroute }

parseArgs :: [String] -> Either Msg (Text, Text, Maybe RouteMode)
parseArgs = go Nothing Nothing Nothing
  where
    go a p r [] = case (a, p) of
      (Just a', Just p') -> Right (a', p', r)
      _                  -> Left MsgExpectedPair
    go a p _ ("--route" : v : xs) = case parseRoute (T.pack v) of
      Nothing -> Left (MsgUnknownRoute (T.pack v))
      Just r' -> go a p (Just r') xs
    go _ _ _ ["--route"]    = Left MsgRouteNeedsValue
    go Nothing p r (x : xs) = go (Just (T.pack x)) p r xs
    go a Nothing r (x : xs) = go a (Just (T.pack x)) r xs
    go _ _ _ (x : _)        = Left (MsgUnexpectedArg (T.pack x))

planCmd :: Lang -> Agent -> Provider -> Config -> IO ()
planCmd lang a p cfg = case previewSession cfg a p of
  Left err -> die lang err
  Right ls -> mapM_ (TIO.putStrLn . plText) ls

runCmd :: Lang -> Agent -> Provider -> Config -> IO ()
runCmd lang a p cfg =
  runSession (TIO.hPutStrLn stderr . ("[proxy-tool] " <>)) cfg a p >>= \case
    Left err  -> die lang err
    Right out -> exitWith (soExit out)

-- ---------------------------------------------------------------------------
-- serve
-- ---------------------------------------------------------------------------

data ServeArgs = ServeArgs
  { saProvider :: Text
  , saPort     :: Int
  , saTokenEnv :: Text
  , saStdinEof :: Bool
  }

-- | @serve PROVIDER --port N --token-env NAME [--exit-on-stdin-eof]@.
--
-- The port is required and fixed: a standing proxy is found by whoever
-- started it at a known address, so an OS-chosen port would be unreachable.
-- The token is named, never given: a value on the command line would be
-- visible to every process that can list command lines.
parseServeArgs :: [String] -> Either Msg ServeArgs
parseServeArgs = go Nothing Nothing Nothing False
  where
    go (Just p) (Just n) (Just e) s [] = Right (ServeArgs p n e s)
    go _ _ _ _ [] = Left MsgServeExpected
    go _ _ _ _ ["--port"] = Left MsgServeExpected
    go _ _ _ _ ["--token-env"] = Left MsgServeExpected
    go p _ e s ("--port" : v : xs) = case reads v of
      [(n, "")] | n >= 1 && n <= 65535 -> go p (Just n) e s xs
      _ -> Left (MsgServeBadPort (T.pack v))
    go p n _ s ("--token-env" : v : xs)
      | not (null v) = go p n (Just (T.pack v)) s xs
      | otherwise    = Left MsgServeExpected
    go p n e _ ("--exit-on-stdin-eof" : xs) = go p n e True xs
    go Nothing n e s (x : xs)
      | not ("--" `isPrefixOf` x) = go (Just (T.pack x)) n e s xs
    go _ _ _ _ (x : _) = Left (MsgUnexpectedArg (T.pack x))

serveCmd :: Lang -> Config -> [String] -> IO ()
serveCmd lang cfg rest = case parseServeArgs rest of
  Left err -> dieWithUsage lang err
  Right sa -> case findProvider (saProvider sa) cfg of
    Nothing -> die lang (MsgNoSuchProvider (saProvider sa))
    Just p -> tokenFromEnv (saTokenEnv sa) >>= \case
      Left err -> die lang err
      Right token -> do
        stop <- newEmptyMVar
        let fire = void (tryPutMVar stop ())
        installStopHandlers fire
        when (saStdinEof sa) (void (forkIO (drainStdin >> fire)))
        serveSession logLine cfg p (saPort sa) token (const (pure ())) fire
                     (waitForStop (takeMVar stop)) >>= \case
          Left err -> die lang err
          Right () -> exitWith ExitSuccess
  where
    logLine = TIO.hPutStrLn stderr . ("[proxy-tool] " <>)

-- | @serve-stop --port N --token-env NAME@: stop a standing proxy that has
-- no console to send Ctrl-C to (one a supervisor started detached), through
-- its authorised @POST \/admin\/shutdown@.  Exit 0 once the port has gone
-- quiet; 1 when the proxy could not be stopped (nothing listening, the
-- token refused, still answering); 2 for a wrong command line or an unset
-- token variable.
serveStopCmd :: Lang -> [String] -> IO ()
serveStopCmd lang rest = case parse Nothing Nothing rest of
  Left err -> dieWithUsage lang err
  Right (port, envName) -> tokenFromEnv envName >>= \case
    Left err -> die lang err
    Right token -> stopServe port token >>= \case
      Right () -> do
        TIO.putStrLn (render lang (MsgServeStopDone port))
        exitWith ExitSuccess
      Left err -> do
        TIO.hPutStrLn stderr (render lang err)
        exitWith (ExitFailure 1)
  where
    parse (Just p) (Just e) [] = Right (p, e)
    parse _ _ [] = Left MsgServeStopExpected
    parse _ e ("--port" : v : xs) = case reads v of
      [(n, "")] | n >= 1 && n <= 65535 -> parse (Just n) e xs
      _ -> Left (MsgServeBadPort (T.pack v))
    parse p _ ("--token-env" : v : xs)
      | not (null v) = parse p (Just (T.pack v)) xs
    parse _ _ (x : _) = Left (MsgUnexpectedArg (T.pack x))

-- | Block until stopped.  The stop handlers below fill the MVar; a Ctrl-C
-- that reaches the main thread as the RTS's own 'UserInterrupt' (before the
-- handlers are installed, or on a runtime that delivers it that way) is the
-- same request to stop, so it ends the wait instead of the process -- the
-- proxy is then closed by 'serveSession' and the exit status is 0.
waitForStop :: IO () -> IO ()
waitForStop wait = handle onAsync wait
  where
    onAsync UserInterrupt = pure ()
    onAsync e             = throwIO e

-- | Read stdin to its end, then return (@--exit-on-stdin-eof@).
--
-- Opt-in, because a supervisor that wants the proxy to outlive it starts it
-- with stdin on the null device, which is at end-of-file immediately.  A read
-- error means stdin is gone too, so it ends the wait the same way.
drainStdin :: IO ()
drainStdin = void (try loop :: IO (Either IOException ()))
  where
    loop = do
      hSetBinaryMode stdin True
      let go = do
            chunk <- BS.hGetSome stdin 4096
            unless (BS.null chunk) go
      go

-- | Turn the platform's stop signals into one stop request: Ctrl-C,
-- Ctrl-Break, console close, logoff and shutdown on Windows; SIGINT and
-- SIGTERM elsewhere.
installStopHandlers :: IO () -> IO ()
#if defined(mingw32_HOST_OS)
installStopHandlers fire =
  void (Console.installHandler (Console.Catch (const fire)))
#else
installStopHandlers fire = do
  void (Signals.installHandler Signals.sigINT (Signals.Catch fire) Nothing)
  void (Signals.installHandler Signals.sigTERM (Signals.Catch fire) Nothing)
#endif

-- ---------------------------------------------------------------------------
-- doctor
-- ---------------------------------------------------------------------------

-- | Check everything that can be checked without contacting a provider:
-- the config parses, each agent's command exists, each provider's credential
-- is obtainable, and each pairing either plans or explains itself.
doctor :: Lang -> Config -> FilePath -> IO ()
doctor lang cfg path = do
  heading MsgDocConfig (T.pack path)
  heading MsgDocRoute (routeLabel (cfgRoute cfg))
  heading MsgDocProxyPort $
    maybe (say MsgDocAutoPort) (T.pack . show) (cfgPort cfg)
  heading MsgDocLanguage (langLabel lang)
  TIO.putStrLn ""
  TIO.putStrLn (say MsgDocAgents)
  agentOk <- mapM checkAgent (cfgAgents cfg)
  TIO.putStrLn ""
  TIO.putStrLn (say MsgDocProviders)
  provOk <- mapM checkProvider (cfgProviders cfg)
  TIO.putStrLn ""
  TIO.putStrLn (say MsgDocPairings)
  mapM_ checkPair [ (a, p) | a <- cfgAgents cfg, p <- cfgProviders cfg ]
  TIO.putStrLn ""
  let bad = length (filter not agentOk) + length (filter not provOk)
  TIO.putStrLn $ if bad == 0
    then say MsgDocAllOk
    else say (MsgDocNeedAttention bad)
  exitWith ExitSuccess
  where
    say = render lang

    headingWidth = maximum (map (textWidth . say)
      [MsgDocConfig, MsgDocRoute, MsgDocProxyPort, MsgDocLanguage])
    heading m v = TIO.putStrLn (padWidth headingWidth (say m) <> " : " <> v)

    checkAgent a = resolveCommand (agCommand a) >>= \case
      Left err -> do
        TIO.putStrLn ("  [ ] " <> pad 10 (agId a) <> say err)
        pure False
      Right res -> do
        TIO.putStrLn ("  [x] " <> pad 10 (agId a) <> crVia res)
        pure True

    checkProvider p = resolveSecret (provKey p) >>= \case
      Left err -> do
        TIO.putStrLn ("  [ ] " <> pad 22 (provId p) <> say err)
        pure False
      Right k -> do
        TIO.putStrLn $ "  [x] " <> pad 22 (provId p)
                    <> (if T.null k
                          then say MsgDocNoCred
                          else say (MsgDocCredAvailable
                                      (secretDisplay (provKey p))))
        pure True

    -- The route is asked for as a fact, not scraped back out of the rendered
    -- preview: once the labels are translated there is no prefix to match on.
    checkPair (a, p) = TIO.putStrLn $
      case planLaunch a p (cfgRoute cfg) of
        Left err -> "  [ ] " <> pairName a p <> say err
        Right plan -> "  [x] " <> pairName a p
                   <> routeLabel (lpRoute plan) <> " -> "
                   <> say (routeSummary plan)

    pairName a p = pad 26 (agId a <> " -> " <> provId p)
