{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration: bring up the proxy if the pairing needs one, run the
-- agent attached to it, and tear everything down again.
--
-- This is the only place where "ProxyTool.Launch" and "ProxyTool.Proxy" meet.
-- Keeping it separate means the TUI and the command line share exactly one
-- implementation of what it means to start a session.
module ProxyTool.Session
  ( SessionOutcome (..)
  , runSession
  , previewSession
  , translateOptsFor
  ) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode)

import ProxyTool.Config
import ProxyTool.Launch
import ProxyTool.Proxy
import ProxyTool.I18n
import ProxyTool.Translate (TranslateOpts (..))
import ProxyTool.Types

data SessionOutcome = SessionOutcome
  { soPlan     :: LaunchPlan
  , soResolved :: ResolvedLaunch
  , soPort     :: Maybe Int
  , soExit     :: ExitCode
  }

-- | What the proxy should impose, derived from the provider.
--
-- An empty model means \"leave whatever the agent asked for alone\", which is
-- different from forcing the empty string.
translateOptsFor :: Provider -> TranslateOpts
translateOptsFor p = TranslateOpts
  { optModel     = nonEmpty (provModel p)
  , optFastModel = provFastModel p >>= nonEmpty
  , optEffort    = provEffort p
  , optExtraBody = provExtraBody p
  }
  where
    nonEmpty t = if T.null (T.strip t) then Nothing else Just t

-- | Render what a launch would do, without doing any of it.
previewSession :: Config -> Agent -> Provider -> Either Msg [PreviewLine]
previewSession cfg agent provider = do
  plan <- planLaunch agent provider (cfgRoute cfg)
  let endpoint = previewEndpoint plan (cfgPort cfg)
      -- The preview must never show a real credential, so it is rendered with
      -- a placeholder in place of the session token or provider key.
      resolved = resolveLaunch plan endpoint "<session-token>"
  pure (describeLaunch (cfgLang cfg) plan resolved)

-- | Run a full session and wait for the agent to exit.
runSession
  :: (Text -> IO ())
  -> Config
  -> Agent
  -> Provider
  -> IO (Either Msg SessionOutcome)
runSession logLine cfg agent provider =
  case planLaunch agent provider (cfgRoute cfg) of
    Left err -> pure (Left err)
    Right plan -> resolveSecret (provKey provider) >>= \case
      Left err -> pure (Left err)
      Right key
        | lpUseProxy plan -> withProxy plan key
        | otherwise       -> run plan Nothing (stripSlash (provBaseUrl provider)) key
  where
    stripSlash = T.dropWhileEnd (== '/')

    withProxy plan key = do
      token <- newSessionToken
      let pc = ProxyConfig
            { pcUpstreamBase = stripSlash (provBaseUrl provider)
            , pcUpstreamWire = provWire provider
            , pcClientWire   = agWire agent
            , pcApiKey       = key
            , pcHeaders      = provHeaders provider
            , pcOpts         = translateOptsFor provider
            , pcToken        = token
            , pcLog          = logLine
            , pcLogBodies    = cfgLogBodies cfg
            , pcLang         = cfgLang cfg
            }
      startProxy pc (cfgPort cfg) >>= \case
        Left err -> pure (Left err)
        Right h -> bracket (pure h) stopProxy $ \handle -> do
          let endpoint = "http://127.0.0.1:" <> T.pack (show (phPort handle))
          let say = render (cfgLang cfg)
              mode = if lpTranslate plan
                       then say (MsgProxyModeTranslate (wireLabel (agWire agent))
                                                       (wireLabel (provWire provider)))
                       else say MsgProxyModeRewrite
          logLine $ say (MsgProxyListening endpoint
                                           (stripSlash (provBaseUrl provider)))
                 <> " (" <> mode <> ")"
          -- The agent is handed the session token, not the provider key: the
          -- credential stays inside this process.
          run plan (Just (phPort handle)) endpoint token

    run plan mport endpoint agentKey = do
      let resolved = resolveLaunch plan endpoint agentKey
      mapM_ (\w -> logLine (render (cfgLang cfg) (MsgLblWarning)
                            <> ": " <> render (cfgLang cfg) w)) (lpWarnings plan)
      executeAgent resolved >>= \case
        Left err   -> pure (Left err)
        Right code -> pure (Right (SessionOutcome plan resolved mport code))
