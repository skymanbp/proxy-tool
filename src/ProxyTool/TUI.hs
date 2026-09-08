{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The terminal UI.
--
-- Two panes pick an agent and a provider; a form edits a provider's
-- endpoint, model and effort; a second form edits how an agent is bound to an
-- endpoint.  Launching suspends the interface, hands the terminal to the
-- agent, and takes it back when the agent exits.
--
-- Forms are built from a list of rows rather than a field-by-field case
-- split, so navigation, rendering and key handling each exist once and adding
-- a field is a one-line change.
--
-- No display string is written here: every one of them is a 'Msg' rendered in
-- the current language.  Statuses and validation errors are /stored/
-- unrendered, so pressing the language key re-says whatever is already on
-- screen instead of leaving a stale sentence behind in the old language.
module ProxyTool.TUI
  ( runTui
  ) where

-- Brick's own @render@ (a 'Widget' field) and @textWidth@ (which counts
-- characters, not columns) are hidden: this module wants the message renderer
-- and the CJK-aware width from "ProxyTool.I18n", and neither Brick name is
-- used here.
import Brick hiding (render, textWidth)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Edit
  ( Editor
  , editAttr
  , editFocusedAttr
  , editorText
  , getEditContents
  , handleEditorEvent
  , renderEditor
  )
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, Value (Object), decodeStrict, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as V
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hFlush, stdout)

import ProxyTool.Config
import ProxyTool.I18n
import ProxyTool.Launch (PreviewLine (..))
import ProxyTool.Session
import ProxyTool.Types

-- ---------------------------------------------------------------------------
-- Names, panes, fields
-- ---------------------------------------------------------------------------

data Name
  = NProvEdit PField
  | NAgentEdit AField
  deriving (Eq, Ord, Show)

data Pane = PaneAgents | PaneProviders
  deriving (Eq, Show)

-- | Editable text fields of a provider.
data PField
  = PLabel | PBaseUrl | PModel | PFastModel | PKey | PHeaders | PExtraBody
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | A row of the provider form.  The two choice rows are not text, so they
-- are cycled rather than typed into.
data PRow = PRText PField | PRWire | PREffort
  deriving (Eq, Show)

providerRows :: [PRow]
providerRows =
  [ PRText PLabel, PRWire, PRText PBaseUrl, PRText PKey
  , PRText PModel, PRText PFastModel, PREffort
  , PRText PHeaders, PRText PExtraBody
  ]

pFieldLabel :: PField -> Msg
pFieldLabel = \case
  PLabel     -> MsgFldLabel
  PBaseUrl   -> MsgFldBaseUrl
  PModel     -> MsgFldModel
  PFastModel -> MsgFldFastModel
  PKey       -> MsgFldKey
  PHeaders   -> MsgFldHeaders
  PExtraBody -> MsgFldExtraBody

pFieldHint :: PField -> Msg
pFieldHint = \case
  PLabel     -> MsgHintLabel
  PBaseUrl   -> MsgHintBaseUrl
  PModel     -> MsgHintModel
  PFastModel -> MsgHintFastModel
  PKey       -> MsgHintKey
  PHeaders   -> MsgHintHeaders
  PExtraBody -> MsgHintExtraBody

-- | Editable text fields of an agent binding.
data AField
  = ALabel | ACommand | AArgs | ABaseUrlEnv | AKeyEnv | AModelEnv
  | AFastModelEnv | AExtraEnv
  deriving (Eq, Ord, Enum, Bounded, Show)

data ARow = ARText AField | ARWire
  deriving (Eq, Show)

agentRows :: [ARow]
agentRows =
  [ ARText ALabel, ARText ACommand, ARWire, ARText ABaseUrlEnv
  , ARText AKeyEnv, ARText AModelEnv, ARText AFastModelEnv
  , ARText AExtraEnv, ARText AArgs
  ]

aFieldLabel :: AField -> Msg
aFieldLabel = \case
  ALabel        -> MsgFldAgLabel
  ACommand      -> MsgFldAgCommand
  AArgs         -> MsgFldAgArgs
  ABaseUrlEnv   -> MsgFldAgBaseUrlEnv
  AKeyEnv       -> MsgFldAgKeyEnv
  AModelEnv     -> MsgFldAgModelEnv
  AFastModelEnv -> MsgFldAgFastModelEnv
  AExtraEnv     -> MsgFldAgExtraEnv

aFieldHint :: AField -> Msg
aFieldHint = \case
  ALabel        -> MsgHintAgLabel
  ACommand      -> MsgHintAgCommand
  AArgs         -> MsgHintAgArgs
  ABaseUrlEnv   -> MsgHintAgBaseUrlEnv
  AKeyEnv       -> MsgHintAgKeyEnv
  AModelEnv     -> MsgHintAgModelEnv
  AFastModelEnv -> MsgHintAgFastModelEnv
  AExtraEnv     -> MsgHintAgExtraEnv

-- ---------------------------------------------------------------------------
-- Forms and state
-- ---------------------------------------------------------------------------

data ProviderForm = ProviderForm
  { pfOriginal :: Maybe Text
    -- ^ Id being edited; 'Nothing' for a new provider.
  , pfEdits    :: M.Map PField (Editor Text Name)
  , pfWire     :: Wire
  , pfEffort   :: Maybe Effort
  , pfRow      :: Int
  , pfError    :: Maybe Msg
  }

data AgentForm = AgentForm
  { afOriginal :: Text
  , afEdits    :: M.Map AField (Editor Text Name)
  , afWire     :: Wire
  , afRow      :: Int
  , afError    :: Maybe Msg
  }

data Screen
  = ScrMain
  | ScrProvider ProviderForm
  | ScrAgent AgentForm
  | ScrHelp

data St = St
  { stCfg    :: Config
  , stPath   :: FilePath
  , stLang   :: Lang
    -- ^ The language actually being rendered.  It starts from the effective
    -- language the caller resolved, which may come from an override that must
    -- not be written back to the file; only the language key changes
    -- 'cfgLang'.
  , stPane   :: Pane
  , stAgIx   :: Int
  , stPrIx   :: Int
  , stScreen :: Screen
  , stStatus :: Msg
  , stDirty  :: Bool
  }

say :: St -> Msg -> Text
say st = render (stLang st)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Run the interface.  The 'Lang' is the effective language for this run;
-- the returned config carries whatever should be persisted.
runTui :: FilePath -> Lang -> Config -> IO Config
runTui path lang cfg0 = do
  let st0 = St
        { stCfg    = cfg0
        , stPath   = path
        , stLang   = lang
        , stPane   = PaneAgents
        , stAgIx   = indexOf (cfgLastAgent cfg0) (map agId (cfgAgents cfg0))
        , stPrIx   = indexOf (cfgLastProvider cfg0) (map provId (cfgProviders cfg0))
        , stScreen = ScrMain
        , stStatus = MsgStatusConfig (T.pack path)
        , stDirty  = False
        }
  stFinal <- defaultMain app st0
  pure (stCfg stFinal)
  where
    indexOf mid ids = fromMaybe 0 $ do
      i <- mid
      lookup i (zip ids [0 ..])

app :: App St e Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "title",     fg V.cyan)
  , (attrName "sel",       V.black `on` V.cyan)
  , (attrName "selDim",    V.black `on` V.white)
  , (attrName "key",       fg V.yellow)
  , (attrName "dim",       fg V.brightBlack)
  , (attrName "warn",      fg V.yellow)
  , (attrName "err",       fg V.red)
  , (attrName "ok",        fg V.green)
  , (editAttr,             V.white `on` V.black)
  , (editFocusedAttr,      V.black `on` V.brightWhite)
  ]

-- ---------------------------------------------------------------------------
-- Selection helpers
-- ---------------------------------------------------------------------------

currentAgent :: St -> Maybe Agent
currentAgent st = nth (stAgIx st) (cfgAgents (stCfg st))

currentProvider :: St -> Maybe Provider
currentProvider st = nth (stPrIx st) (cfgProviders (stCfg st))

nth :: Int -> [a] -> Maybe a
nth i xs
  | i >= 0, i < length xs = Just (xs !! i)
  | otherwise             = Nothing

clampIx :: Int -> [a] -> Int
clampIx i xs
  | null xs   = 0
  | i < 0     = 0
  | i >= n    = n - 1
  | otherwise = i
  where n = length xs

-- ---------------------------------------------------------------------------
-- Drawing
-- ---------------------------------------------------------------------------

drawUI :: St -> [Widget Name]
drawUI st = case stScreen st of
  ScrMain       -> [withBorderStyle unicode (mainScreen st)]
  ScrProvider f -> [withBorderStyle unicode (providerScreen st f)]
  ScrAgent f    -> [withBorderStyle unicode (agentScreen st f)]
  ScrHelp       -> [withBorderStyle unicode (helpScreen st)]

mainScreen :: St -> Widget Name
mainScreen st = vBox
  [ hBox
      [ hLimitPercent 50 $ pickList st (stPane st == PaneAgents) MsgPaneAgents
          [ agLabel a <> "  " <> dimSuffix (wireSlug (agWire a))
          | a <- cfgAgents (stCfg st) ]
          (stAgIx st)
      , vBorder
      , pickList st (stPane st == PaneProviders) MsgPaneProviders
          [ provLabel p <> "  " <> dimSuffix (wireSlug (provWire p))
          | p <- cfgProviders (stCfg st) ]
          (stPrIx st)
      ]
  , previewPane st
  , statusBar st
  , keyBar
      [ ("Tab", say st MsgKeyPane),  ("Enter", say st MsgKeyLaunch)
      , ("n", say st MsgKeyNew),     ("e", say st MsgKeyEdit)
      , ("d", say st MsgKeyDelete),  ("r", say st MsgKeyRoute)
      , ("b", say st MsgKeyBinding), ("L", say st MsgKeyLang)
      , ("s", say st MsgKeySave),    ("?", say st MsgKeyHelp)
      , ("q", say st MsgKeyQuit)
      ]
  ]
  where dimSuffix t = "(" <> t <> ")"

pickList :: St -> Bool -> Msg -> [Text] -> Int -> Widget Name
pickList st focused title items sel =
  borderWithLabel
    (withAttr (attrName (if focused then "title" else "dim"))
              (txt (say st title))) $
    padBottom Max $ vBox $
      if null items
        then [withAttr (attrName "dim") (txt ("  " <> say st MsgListEmpty))]
        else [ row i t | (i, t) <- zip [0 ..] items ]
  where
    row i t
      | i == sel  = withAttr (attrName (if focused then "sel" else "selDim"))
                      (padRight Max (txt ("> " <> t)))
      | otherwise = padRight Max (txt ("  " <> t))

previewPane :: St -> Widget Name
previewPane st =
  borderWithLabel (withAttr (attrName "title") (txt (say st MsgLaunchPreview))) $
    padBottom Max $ vBox body
  where
    body = case (currentAgent st, currentProvider st) of
      (Nothing, _) -> [dimTxt (say st MsgNoAgentSelected)]
      (_, Nothing) -> [dimTxt (say st MsgNoProviderSelected)]
      (Just a, Just p) -> case previewSession (stCfg st) a p of
        Left err -> [ withAttr (attrName "err")
                        (txt (say st (MsgCannotLaunch (say st err)))) ]
        Right ls -> map lineWidget ls

    dimTxt = withAttr (attrName "dim") . txt

    -- The warning flag travels on the line itself, so highlighting does not
    -- depend on what the label happens to say in the current language.
    lineWidget l
      | plWarning l = withAttr (attrName "warn") (txt (plText l))
      | otherwise   = txt (plText l)

statusBar :: St -> Widget Name
statusBar st = padRight Max $ hBox
  [ withAttr (attrName (if stDirty st then "warn" else "dim"))
      (txt (say st (if stDirty st then MsgUnsavedFlag else MsgSavedFlag)))
  , txt "  route="
  , withAttr (attrName "key") (txt (routeLabel (cfgRoute (stCfg st))))
  , txt "  lang="
  , withAttr (attrName "key") (txt (langSlug (stLang st)))
  , txt "   "
  , withAttr (attrName "dim") (txt (say st (stStatus st)))
  ]

keyBar :: [(Text, Text)] -> Widget Name
keyBar ks = padRight Max $ hBox $ concat
  [ [ withAttr (attrName "key") (txt k), txt (":" <> d <> "  ") ]
  | (k, d) <- ks ]

-- ---------------------------------------------------------------------------
-- Form drawing
-- ---------------------------------------------------------------------------

formScreen :: St -> Text -> [Widget Name] -> Maybe Msg -> Msg -> Widget Name
formScreen st title rows err hint = vBox
  [ borderWithLabel (withAttr (attrName "title") (txt title)) $
      padBottom Max (vBox rows)
  , case err of
      Just e  -> withAttr (attrName "err")
                   (padRight Max (txt (say st (MsgErrorPrefix (say st e)))))
      Nothing -> withAttr (attrName "dim") (padRight Max (txt (say st hint)))
  , keyBar
      [ ("Tab/Up/Down", say st MsgKeyField)
      , ("Left/Right",  say st MsgKeyChoice)
      , ("Enter",       say st MsgKeySave)
      , ("Esc",         say st MsgKeyCancel)
      ]
  ]

-- | One labelled row.  The label column is as wide as the widest label in the
-- language being rendered, measured in terminal columns rather than
-- characters, so the values still line up once the labels are Chinese.
formRow :: Int -> Bool -> Text -> Widget Name -> Widget Name
formRow width focused label content = hBox
  [ withAttr (attrName (if focused then "key" else "dim")) $
      txt ((if focused then "> " else "  ") <> padWidth width label)
  , txt " "
  , content
  ]

editRow :: Int -> Bool -> Text -> Editor Text Name -> Widget Name
editRow width focused label ed =
  formRow width focused label
    (vLimit 1 (renderEditor (txt . T.concat) focused ed))

choiceRow :: Int -> Bool -> Text -> Text -> Widget Name
choiceRow width focused label value =
  formRow width focused label $ padRight Max $
    withAttr (attrName "ok") (txt ("< " <> value <> " >"))

providerScreen :: St -> ProviderForm -> Widget Name
providerScreen st f = formScreen st title rows (pfError f) hint
  where
    title = case pfOriginal f of
      Nothing  -> say st MsgNewProvider
      Just pid -> say st (MsgEditProvider pid)
    width = maximum (map (textWidth . say st) labels)
    labels = MsgFldDialect : MsgFldEffort
           : map pFieldLabel [minBound .. maxBound]
    rows = [ renderRow i r | (i, r) <- zip [0 ..] providerRows ]
    renderRow i = \case
      PRText fld -> editRow width (i == pfRow f) (say st (pFieldLabel fld))
                      (pfEdits f M.! fld)
      PRWire     -> choiceRow width (i == pfRow f) (say st MsgFldDialect)
                      (wireLabel (pfWire f))
      PREffort   -> choiceRow width (i == pfRow f) (say st MsgFldEffort)
                      (maybe (say st MsgValueUnset) effortSlug (pfEffort f))
    hint = case nth (pfRow f) providerRows of
      Just (PRText fld) -> pFieldHint fld
      Just PRWire       -> MsgHintDialect
      Just PREffort     -> MsgHintEffort
      Nothing           -> MsgHintLabel

agentScreen :: St -> AgentForm -> Widget Name
agentScreen st f =
  formScreen st (say st (MsgEditAgent (afOriginal f))) rows (afError f) hint
  where
    width = maximum (map (textWidth . say st) labels)
    labels = MsgFldDialect : map aFieldLabel [minBound .. maxBound]
    rows = [ renderRow i r | (i, r) <- zip [0 ..] agentRows ]
    renderRow i = \case
      ARText fld -> editRow width (i == afRow f) (say st (aFieldLabel fld))
                      (afEdits f M.! fld)
      ARWire     -> choiceRow width (i == afRow f) (say st MsgFldDialect)
                      (wireLabel (afWire f))
    hint = case nth (afRow f) agentRows of
      Just (ARText fld) -> aFieldHint fld
      Just ARWire       -> MsgHintAgDialect
      Nothing           -> MsgHintAgLabel

helpScreen :: St -> Widget Name
helpScreen st =
  borderWithLabel (withAttr (attrName "title") (txt "proxy-tool")) $
    padBottom Max $ vBox $
      map txt (helpLines (stLang st))
        <> [ hBorder
           , withAttr (attrName "dim")
               (txt ("  " <> say st MsgAnyKeyReturns))
           ]

-- ---------------------------------------------------------------------------
-- Event handling
-- ---------------------------------------------------------------------------

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent ev = do
  st <- get
  case stScreen st of
    ScrMain       -> handleMain ev
    ScrHelp       -> case ev of
      VtyEvent (V.EvKey _ _) -> modify (\s -> s { stScreen = ScrMain })
      _                      -> pure ()
    ScrProvider f -> handleProviderForm f ev
    ScrAgent f    -> handleAgentForm f ev

handleMain :: BrickEvent Name e -> EventM Name St ()
handleMain (VtyEvent (V.EvKey key mods)) = case (key, mods) of
  (V.KChar 'q', [])  -> saveAndQuit
  (V.KEsc,      [])  -> saveAndQuit
  (V.KChar 'c', [V.MCtrl]) -> halt
  (V.KChar '?', [])  -> modify (\s -> s { stScreen = ScrHelp })
  (V.KChar '\t', []) -> modify togglePane
  (V.KBackTab,  [])  -> modify togglePane
  (V.KUp,       [])  -> modify (moveSel (-1))
  (V.KDown,     [])  -> modify (moveSel 1)
  (V.KChar 'k', [])  -> modify (moveSel (-1))
  (V.KChar 'j', [])  -> modify (moveSel 1)
  (V.KEnter,    [])  -> launchSelected
  (V.KChar 'r', [])  -> modify cycleRoute
  (V.KChar 'L', [])  -> modify cycleLang
  (V.KChar 'l', [])  -> modify cycleLang
  (V.KChar 'n', [])  -> modify (\s -> s { stScreen = ScrProvider newProviderForm })
  (V.KChar 'e', [])  -> modify openProviderForm
  (V.KChar 'b', [])  -> modify openAgentForm
  (V.KChar 'd', [])  -> modify deleteSelectedProvider
  (V.KChar 's', [])  -> saveNow
  _                  -> pure ()
handleMain _ = pure ()

togglePane :: St -> St
togglePane s = s { stPane = case stPane s of
                     PaneAgents    -> PaneProviders
                     PaneProviders -> PaneAgents }

moveSel :: Int -> St -> St
moveSel d s = case stPane s of
  PaneAgents    -> s { stAgIx = clampIx (stAgIx s + d) (cfgAgents (stCfg s)) }
  PaneProviders -> s { stPrIx = clampIx (stPrIx s + d) (cfgProviders (stCfg s)) }

cycleRoute :: St -> St
cycleRoute s = s { stCfg = (stCfg s) { cfgRoute = nextEnum (cfgRoute (stCfg s)) }
                 , stDirty = True }

-- | Switching language is an explicit choice, so unlike an environment
-- override it is written back to the config.
cycleLang :: St -> St
cycleLang s =
  let l = nextEnum (stLang s)
  in s { stLang   = l
       , stCfg    = (stCfg s) { cfgLang = l }
       , stDirty  = True
       , stStatus = MsgStatusLanguage (langLabel l)
       }

nextEnum :: (Eq a, Enum a, Bounded a) => a -> a
nextEnum x = if x == maxBound then minBound else succ x

-- ---------------------------------------------------------------------------
-- Launching
-- ---------------------------------------------------------------------------

-- | Hand the terminal to the agent, then take it back.
--
-- 'suspendAndResume'' shuts the interface down before the child starts, so
-- the agent gets a clean terminal and its own full-screen UI works normally.
launchSelected :: EventM Name St ()
launchSelected = do
  st <- get
  case (currentAgent st, currentProvider st) of
    (Just a, Just p) -> do
      let cfg  = rememberSelection st a p
          lang = stLang st
          out  = TIO.putStrLn . ("[proxy-tool] " <>)
      summary <- suspendAndResume' $ do
        putStrLn ""
        result <- runSession out cfg a p
        case result of
          Left err -> do
            let msg = MsgCannotLaunch (render lang err)
            out (render lang msg)
            waitForReturn lang
            pure (MsgLaunchRefused (render lang err))
          Right res -> do
            let done = MsgExitedWith (agLabel a)
                                     (render lang (describeExit (soExit res)))
            out (render lang done)
            waitForReturn lang
            pure done
      modify $ \s -> s { stCfg = cfg, stDirty = True, stStatus = summary }
    _ -> modify (\s -> s { stStatus = MsgStatusSelectBoth })

-- | Give the user a chance to read the agent's last output before the
-- interface repaints over it.
waitForReturn :: Lang -> IO ()
waitForReturn lang = do
  TIO.putStr ("[proxy-tool] " <> render lang MsgPressEnterReturn <> " ")
  hFlush stdout
  void getLine

describeExit :: ExitCode -> Msg
describeExit = \case
  ExitSuccess   -> MsgExitStatus 0
  ExitFailure n -> MsgExitStatus n

rememberSelection :: St -> Agent -> Provider -> Config
rememberSelection st a p = (stCfg st)
  { cfgLastAgent    = Just (agId a)
  , cfgLastProvider = Just (provId p)
  }

-- ---------------------------------------------------------------------------
-- Saving
-- ---------------------------------------------------------------------------

saveNow :: EventM Name St ()
saveNow = do
  st <- get
  res <- liftIO (saveConfig (stPath st) (stCfg st))
  modify $ \s -> case res of
    Left err -> s { stStatus = MsgStatusSaveFailed (say s err) }
    Right () -> s { stStatus = MsgStatusSaved (T.pack (stPath s))
                  , stDirty = False }

saveAndQuit :: EventM Name St ()
saveAndQuit = do
  st <- get
  if stDirty st
    then do
      res <- liftIO (saveConfig (stPath st) (stCfg st))
      case res of
        Left err -> modify (\s -> s { stStatus = MsgStatusSaveFailed (say s err) })
        Right () -> halt
    else halt

-- ---------------------------------------------------------------------------
-- Provider form
-- ---------------------------------------------------------------------------

newProviderForm :: ProviderForm
newProviderForm = ProviderForm
  { pfOriginal = Nothing
  , pfEdits    = M.fromList
      [ (f, editorText (NProvEdit f) (Just 1) "") | f <- [minBound .. maxBound] ]
  , pfWire     = WireOpenAIChat
  , pfEffort   = Nothing
  , pfRow      = 0
  , pfError    = Nothing
  }

openProviderForm :: St -> St
openProviderForm s = case currentProvider s of
  Nothing -> s { stStatus = MsgStatusNoProviderToEdit }
  Just p  -> s { stScreen = ScrProvider (providerToForm p) }

providerToForm :: Provider -> ProviderForm
providerToForm p = ProviderForm
  { pfOriginal = Just (provId p)
  , pfEdits    = M.fromList
      [ (f, editorText (NProvEdit f) (Just 1) (initial f))
      | f <- [minBound .. maxBound] ]
  , pfWire     = provWire p
  , pfEffort   = provEffort p
  , pfRow      = 0
  , pfError    = Nothing
  }
  where
    initial = \case
      PLabel     -> provLabel p
      PBaseUrl   -> provBaseUrl p
      PModel     -> provModel p
      PFastModel -> fromMaybe "" (provFastModel p)
      PKey       -> renderSecretField (provKey p)
      PHeaders   -> renderHeaders (provHeaders p)
      PExtraBody -> maybe "" (TE.decodeUtf8 . BL.toStrict . encode . Object)
                      (provExtraBody p)

handleProviderForm :: ProviderForm -> BrickEvent Name e -> EventM Name St ()
handleProviderForm f (VtyEvent (V.EvKey key mods)) = case (key, mods) of
  (V.KEsc, [])     -> modify (\s -> s { stScreen = ScrMain })
  (V.KEnter, [])   -> commitProviderForm f
  (V.KChar '\t', [])-> setForm f { pfRow = wrap (pfRow f + 1) providerRows }
  (V.KBackTab, []) -> setForm f { pfRow = wrap (pfRow f - 1) providerRows }
  (V.KDown, [])    -> setForm f { pfRow = wrap (pfRow f + 1) providerRows }
  (V.KUp, [])      -> setForm f { pfRow = wrap (pfRow f - 1) providerRows }
  (V.KLeft, [])    -> onChoice (-1)
  (V.KRight, [])   -> onChoice 1
  _                -> editCurrent
  where
    setForm nf = modify (\s -> s { stScreen = ScrProvider nf })

    onChoice d = case nth (pfRow f) providerRows of
      Just PRWire   -> setForm f { pfWire = cycleEnum d (pfWire f) }
      Just PREffort -> setForm f { pfEffort = cycleMaybeEffort d (pfEffort f) }
      _             -> editCurrent

    editCurrent = case nth (pfRow f) providerRows of
      Just (PRText fld) -> do
        ed' <- nestEventM' (pfEdits f M.! fld)
                 (handleEditorEvent (VtyEvent (V.EvKey key mods)))
        setForm f { pfEdits = M.insert fld ed' (pfEdits f), pfError = Nothing }
      _ -> pure ()
handleProviderForm _ _ = pure ()

commitProviderForm :: ProviderForm -> EventM Name St ()
commitProviderForm f = do
  st <- get
  case buildProvider st f of
    Left err -> modify (\s -> s { stScreen = ScrProvider f { pfError = Just err } })
    Right p  -> modify $ \s ->
      let cfg' = upsertProvider p (stCfg s)
          ix   = fromMaybe (stPrIx s)
                   (lookup (provId p) (zip (map provId (cfgProviders cfg')) [0 ..]))
      in s { stCfg = cfg'
           , stPrIx = ix
           , stScreen = ScrMain
           , stDirty = True
           , stStatus = MsgStatusProviderUpdated (provId p)
           }

buildProvider :: St -> ProviderForm -> Either Msg Provider
buildProvider st f = do
  let val fld = T.strip (T.concat (getEditContents (pfEdits f M.! fld)))
      label   = val PLabel
      baseUrl = val PBaseUrl
  if T.null baseUrl
    then Left MsgErrBaseUrlRequired
    else Right ()
  if any (`T.isPrefixOf` T.toLower baseUrl) ["http://", "https://"]
    then Right ()
    else Left MsgErrBaseUrlScheme
  extra <- parseExtraBody (val PExtraBody)
  let pid = case pfOriginal f of
        Just existing -> existing
        Nothing       -> freshProviderId (stCfg st)
                           (if T.null label then baseUrl else label)
  pure emptyProvider
    { provId        = pid
    , provLabel     = if T.null label then pid else label
    , provWire      = pfWire f
    , provBaseUrl   = baseUrl
    , provKey       = parseSecretField (val PKey)
    , provModel     = val PModel
    , provFastModel = let v = val PFastModel in if T.null v then Nothing else Just v
    , provEffort    = pfEffort f
    , provHeaders   = parseHeaders (val PHeaders)
    , provExtraBody = extra
    }

deleteSelectedProvider :: St -> St
deleteSelectedProvider s = case currentProvider s of
  Nothing -> s { stStatus = MsgStatusNothingToDelete }
  Just p ->
    let cfg' = removeProvider (provId p) (stCfg s)
    in s { stCfg = cfg'
         , stPrIx = clampIx (stPrIx s) (cfgProviders cfg')
         , stDirty = True
         , stStatus = MsgStatusDeleted (provId p)
         }

-- ---------------------------------------------------------------------------
-- Agent form
-- ---------------------------------------------------------------------------

openAgentForm :: St -> St
openAgentForm s = case currentAgent s of
  Nothing -> s { stStatus = MsgStatusNoAgentToEdit }
  Just a  -> s { stScreen = ScrAgent (agentToForm a) }

agentToForm :: Agent -> AgentForm
agentToForm a = AgentForm
  { afOriginal = agId a
  , afEdits    = M.fromList
      [ (f, editorText (NAgentEdit f) (Just 1) (initial f))
      | f <- [minBound .. maxBound] ]
  , afWire     = agWire a
  , afRow      = 0
  , afError    = Nothing
  }
  where
    b = agBinding a
    initial = \case
      ALabel        -> agLabel a
      ACommand      -> agCommand a
      AArgs         -> T.unwords (agArgs a <> bindArgs b)
      ABaseUrlEnv   -> fromMaybe "" (bindBaseUrlEnv b)
      AKeyEnv       -> fromMaybe "" (bindKeyEnv b)
      AModelEnv     -> fromMaybe "" (bindModelEnv b)
      AFastModelEnv -> fromMaybe "" (bindFastModelEnv b)
      AExtraEnv     -> renderPairs (bindEnv b)

handleAgentForm :: AgentForm -> BrickEvent Name e -> EventM Name St ()
handleAgentForm f (VtyEvent (V.EvKey key mods)) = case (key, mods) of
  (V.KEsc, [])      -> modify (\s -> s { stScreen = ScrMain })
  (V.KEnter, [])    -> commitAgentForm f
  (V.KChar '\t', []) -> setForm f { afRow = wrap (afRow f + 1) agentRows }
  (V.KBackTab, [])  -> setForm f { afRow = wrap (afRow f - 1) agentRows }
  (V.KDown, [])     -> setForm f { afRow = wrap (afRow f + 1) agentRows }
  (V.KUp, [])       -> setForm f { afRow = wrap (afRow f - 1) agentRows }
  (V.KLeft, [])     -> onChoice (-1)
  (V.KRight, [])    -> onChoice 1
  _                 -> editCurrent
  where
    setForm nf = modify (\s -> s { stScreen = ScrAgent nf })
    onChoice d = case nth (afRow f) agentRows of
      Just ARWire -> setForm f { afWire = cycleEnum d (afWire f) }
      _           -> editCurrent
    editCurrent = case nth (afRow f) agentRows of
      Just (ARText fld) -> do
        ed' <- nestEventM' (afEdits f M.! fld)
                 (handleEditorEvent (VtyEvent (V.EvKey key mods)))
        setForm f { afEdits = M.insert fld ed' (afEdits f), afError = Nothing }
      _ -> pure ()
handleAgentForm _ _ = pure ()

commitAgentForm :: AgentForm -> EventM Name St ()
commitAgentForm f = do
  st <- get
  let val fld = T.strip (T.concat (getEditContents (afEdits f M.! fld)))
      command = val ACommand
  if T.null command
    then modify (\s -> s { stScreen =
                             ScrAgent f { afError = Just MsgErrCommandRequired } })
    else do
      let updated a
            | agId a /= afOriginal f = a
            | otherwise = a
                { agLabel = let l = val ALabel in if T.null l then agId a else l
                , agCommand = command
                , agWire = afWire f
                , agArgs = []
                , agBinding = (agBinding a)
                    { bindBaseUrlEnv   = optional (val ABaseUrlEnv)
                    , bindKeyEnv       = optional (val AKeyEnv)
                    , bindModelEnv     = optional (val AModelEnv)
                    , bindFastModelEnv = optional (val AFastModelEnv)
                    , bindEnv          = parsePairs (val AExtraEnv)
                    , bindArgs         = filter (not . T.null) (T.words (val AArgs))
                    }
                }
          optional t = if T.null t then Nothing else Just t
          cfg' = (stCfg st) { cfgAgents = map updated (cfgAgents (stCfg st)) }
      modify $ \s -> s { stCfg = cfg'
                       , stScreen = ScrMain
                       , stDirty = True
                       , stStatus = MsgStatusAgentUpdated (afOriginal f)
                       }

-- ---------------------------------------------------------------------------
-- Field parsing and rendering
-- ---------------------------------------------------------------------------

-- | @env:NAME@ reads a variable; anything else is taken literally.  The
-- distinction is explicit rather than guessed, because guessing wrong either
-- leaks a key into the config file or silently sends the literal text
-- \"env:FOO\" as a credential.
parseSecretField :: Text -> SecretRef
parseSecretField t
  | T.null s                = SecretNone
  | Just n <- T.stripPrefix "env:" s = SecretEnv (T.strip n)
  | otherwise               = SecretLiteral s
  where s = T.strip t

renderSecretField :: SecretRef -> Text
renderSecretField = \case
  SecretNone      -> ""
  SecretEnv n     -> "env:" <> n
  SecretLiteral v -> v

parseHeaders :: Text -> [(Text, Text)]
parseHeaders = mapMaybe one . T.splitOn ";"
  where
    one chunk =
      let (k, v) = T.breakOn ":" chunk
      in if T.null v || T.null (T.strip k)
           then Nothing
           else Just (T.strip k, T.strip (T.drop 1 v))

renderHeaders :: [(Text, Text)] -> Text
renderHeaders ps = T.intercalate "; " [k <> ": " <> v | (k, v) <- ps]

parsePairs :: Text -> [(Text, Text)]
parsePairs = mapMaybe one . T.splitOn ";"
  where
    one chunk =
      let (k, v) = T.breakOn "=" chunk
      in if T.null v || T.null (T.strip k)
           then Nothing
           else Just (T.strip k, T.strip (T.drop 1 v))

renderPairs :: [(Text, Text)] -> Text
renderPairs ps = T.intercalate "; " [k <> "=" <> v | (k, v) <- ps]

parseExtraBody :: Text -> Either Msg (Maybe Object)
parseExtraBody t
  | T.null (T.strip t) = Right Nothing
  | otherwise = case decodeStrict (TE.encodeUtf8 t) of
      Just (Object o) -> Right (Just o)
      Just _          -> Left MsgErrExtraBodyObject
      Nothing         -> Left MsgErrExtraBodyJson

-- ---------------------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------------------

wrap :: Int -> [a] -> Int
wrap i xs
  | null xs   = 0
  | otherwise = i `mod` length xs

cycleEnum :: (Eq a, Enum a, Bounded a) => Int -> a -> a
cycleEnum d x =
  let xs = [minBound .. maxBound]
      n  = length xs
      i  = fromMaybe 0 (lookup x (zip xs [0 ..]))
  in xs !! ((i + d) `mod` n)

-- | Effort cycles through \"unset\" as well as the levels, because leaving it
-- alone is a meaningful choice and must be reachable.
cycleMaybeEffort :: Int -> Maybe Effort -> Maybe Effort
cycleMaybeEffort d cur =
  let opts = Nothing : map Just allEfforts
      n    = length opts
      i    = fromMaybe 0 (lookup cur (zip opts [0 ..]))
  in opts !! ((i + d) `mod` n)
