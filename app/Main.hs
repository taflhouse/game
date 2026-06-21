{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when, void)
import Miso hiding ((!!))
import Miso.CSS (style_)
import Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..),
                  object, (.=), (.:), (.:?), withObject)
import Data.List (isPrefixOf)
import Miso.DSL (JSVal, toJSVal, fromJSValUnchecked, jsg, (#))
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Auth
  ( signUpEmail, signInWithPassword, signOut
  , SignUpEmail(..), SignInCredentials(..), Email(..), Password(..)
  , defaultSignOutOptions
  , AuthResponse(..), AuthData(..), Session(..), User(..)
  )
import Supabase.Miso.Database (insert, selectWithFilters, updateTable, InsertOptions(..), FetchOptions(..), UpdateOptions(..), eq)

import Tafl.Types
import Tafl.Rules (BoardVariant(..))
import Tafl.Move  (getPossibleMovesFrom)
import Tafl.Game  (act, initialState)
import Tafl.AI    (AiConfig(..), bestMove)

-- ---------------------------------------------------------------------------
-- JSON instances for game types (orphans — Tafl.Types has no aeson dep)
-- ---------------------------------------------------------------------------

instance ToJSON Coords where
  toJSON (Coords r c) = toJSON [r, c]

instance FromJSON Coords where
  parseJSON v = do
    [r, c] <- parseJSON v
    pure (Coords r c)

instance ToJSON MoveAction where
  toJSON (MoveAction f t) = toJSON [toJSON f, toJSON t]

instance FromJSON MoveAction where
  parseJSON v = do
    [f, t] <- parseJSON v
    pure (MoveAction f t)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data GameMode = LocalMode | AiMode
  deriving (Eq, Show)

data Screen = HomeScreen | SignInScreen | SignUpScreen | ConfigScreen | GameScreen | ReplayScreen
  deriving (Eq, Show)

data GameRecord = GameRecord
  { grId         :: Maybe MisoString
  , grVariant    :: !MisoString
  , grResultDesc :: !MisoString
  , grGameMode   :: !MisoString
  , grPlayedAt   :: !MisoString
  , grWinner     :: Maybe MisoString
  , grAiSide     :: Maybe MisoString
  , grTotalMoves :: !Int
  , grAiDepth    :: Maybe Int
  , grMoves      :: Maybe [MoveAction]
  , grIsPublic   :: !Bool
  } deriving (Eq, Show)

instance FromJSON GameRecord where
  parseJSON = withObject "GameRecord" $ \v ->
    GameRecord
      <$> v .:? "id"
      <*> v .: "variant"
      <*> v .: "result_desc"
      <*> v .: "game_mode"
      <*> v .: "played_at"
      <*> v .: "winner"
      <*> v .: "ai_side"
      <*> v .: "total_moves"
      <*> v .: "ai_depth"
      <*> v .:? "moves"
      <*> (maybe False id <$> v .:? "is_public")

data Model = Model
  { mScreen         :: !Screen
  , mGameMode       :: !GameMode
  , mGameState      :: !GameState
  , mSelected       :: Maybe Coords
  , mValidMoves     :: [Coords]
  , mVariant        :: !BoardVariant
  , mAiSide         :: !Side
  , mAiThinking     :: !Bool
  , mAiDepth        :: !Int
  , mAiNodeLimit    :: !Int
  , mHistory        :: [GameState]
  , mSession        :: Maybe Session
  , mAuthEmail      :: !MisoString
  , mAuthPassword   :: !MisoString
  , mAuthError      :: Maybe MisoString
  , mAuthMessage    :: Maybe MisoString
  , mAuthLoading    :: !Bool
  , mPastGames      :: [GameRecord]
  , mGamesLoading   :: !Bool
  , mConfigExpanded :: !Bool
  , mShowQuoteRef  :: !Bool
  , mToast         :: Maybe MisoString
  , mShowDepthInfo :: !Bool
  , mShowNodesInfo :: !Bool
  , mLocalGames    :: [GameRecord]
  , mMoveList      :: [MoveAction]
  , mReplayGame    :: Maybe GameRecord
  , mReplayStates  :: [GameState]
  , mReplayIndex   :: !Int
  , mGameId        :: Maybe MisoString
  , mIsPublic      :: !Bool
  , mReplayNotFound :: !Bool
  } deriving Show

instance Eq Model where
  a == b = mScreen a == mScreen b
        && mGameMode a == mGameMode b
        && mGameState a == mGameState b
        && mSelected a == mSelected b
        && mValidMoves a == mValidMoves b
        && mVariant a == mVariant b
        && mAiSide a == mAiSide b
        && mAiThinking a == mAiThinking b
        && mAiDepth a == mAiDepth b
        && mAiNodeLimit a == mAiNodeLimit b
        && mHistory a == mHistory b
        && eqSession (mSession a) (mSession b)
        && mAuthEmail a == mAuthEmail b
        && mAuthPassword a == mAuthPassword b
        && mAuthError a == mAuthError b
        && mAuthMessage a == mAuthMessage b
        && mAuthLoading a == mAuthLoading b
        && mPastGames a == mPastGames b
        && mGamesLoading a == mGamesLoading b
        && mConfigExpanded a == mConfigExpanded b
        && mShowQuoteRef a == mShowQuoteRef b
        && mToast a == mToast b
        && mShowDepthInfo a == mShowDepthInfo b
        && mShowNodesInfo a == mShowNodesInfo b
        && mLocalGames a == mLocalGames b
        && mMoveList a == mMoveList b
        && mReplayGame a == mReplayGame b
        && mReplayStates a == mReplayStates b
        && mReplayIndex a == mReplayIndex b
        && mGameId a == mGameId b
        && mIsPublic a == mIsPublic b
        && mReplayNotFound a == mReplayNotFound b

eqSession :: Maybe Session -> Maybe Session -> Bool
eqSession Nothing Nothing = True
eqSession (Just x) (Just y) = sessionAccessToken x == sessionAccessToken y
eqSession _ _ = False

-- ---------------------------------------------------------------------------
-- Action
-- ---------------------------------------------------------------------------

data Action
  = CellClicked Coords
  | NoOp
  | AiMoveComplete MoveAction
  | SetGameMode GameMode
  | SetVariant BoardVariant
  | SetAiSide Side
  | SetAiDepth Int
  | SetAiNodeLimit Int
  | GotoMove Int
  | StartGame
  | GotoHome
  | GotoSignIn
  | GotoSignUp
  | GotoConfig
  | ToggleConfigExpand
  | HandleURI URI
  | Undo
  | SetAuthEmail MisoString
  | SetAuthPassword MisoString
  | DoSignUp
  | DoSignIn
  | DoSignOut
  | AuthSuccess AuthResponse
  | AuthError MisoString
  | SignOutSuccess Value
  | SessionRestored (Maybe Session)
  | GameSaved Value
  | GameSaveError MisoString
  | GamesLoaded Value
  | GamesLoadError MisoString
  | ToggleTheme
  | ToggleQuoteRef
  | ShowToast MisoString
  | DismissToast
  | ToggleDepthInfo
  | ToggleNodesInfo
  | LocalGamesLoaded [GameRecord]
  | DoMigrateGames MisoString [GameRecord]
  | GotoReplay MisoString
  | ReplayLoaded Value
  | ReplayLoadError MisoString
  | ReplayGotoMove Int
  | InitGame MisoString
  | GameCreated Value
  | GameCreateError MisoString
  | ToggleGamePublic
  | GameUpdated Value
  | GameUpdateError MisoString
  | CopyGameLink
  deriving Show

-- ---------------------------------------------------------------------------
-- Routing
-- ---------------------------------------------------------------------------

data Route = HomeRoute | SignInRoute | SignUpRoute | ConfigRoute
           | PlayRoute MisoString      -- /play/<uuid> active game
           | GameRoute MisoString      -- /games/<uuid> replay/permalink

variantSlug :: BoardVariant -> MisoString
variantSlug = \case
  Brandubh      -> "brandubh"
  Tablut        -> "tablut"
  Classic       -> "copenhagen"
  Line          -> "line"
  Tawlbwrdd     -> "tawlbwrdd"
  Lewis         -> "lewis"
  Parlett       -> "parlett"
  DamienWalker  -> "damien-walker"
  AleaEvangelii -> "alea-evangelii"

variantName :: BoardVariant -> MisoString
variantName = \case
  Brandubh      -> "Brandubh 7x7"
  Tablut        -> "Tablut 9x9"
  Classic       -> "Copenhagen 11x11"
  Line          -> "Line 11x11"
  Tawlbwrdd     -> "Tawlbwrdd 11x11"
  Lewis         -> "Lewis"
  Parlett       -> "Parlett"
  DamienWalker  -> "Damien Walker"
  AleaEvangelii -> "Alea Evangelii"

parseRoute :: URI -> Route
parseRoute uri = case uriPath uri of
  "sign-in"  -> SignInRoute
  "sign-up"  -> SignUpRoute
  "new-game" -> ConfigRoute
  path
    | Just uuid <- msStripPrefix "play/" path
    , isUUID uuid -> PlayRoute uuid
    | Just uuid <- msStripPrefix "games/" path
    , isUUID uuid -> GameRoute uuid
    | otherwise   -> HomeRoute

msStripPrefix :: String -> MisoString -> Maybe MisoString
msStripPrefix pfx s =
  let str = fromMisoString s :: String
  in if pfx `isPrefixOf` str
     then Just (ms (drop (length pfx) str))
     else Nothing

isUUID :: MisoString -> Bool
isUUID s =
  let str = fromMisoString s :: String
  in length str == 36 && all (\c -> c `elem` ("0123456789abcdef-" :: [Char])) str

lookupVariant :: MisoString -> Maybe BoardVariant
lookupVariant slug = lookup slug [ (variantSlug v, v) | v <- [minBound .. maxBound] ]

friendlyAuthError :: MisoString -> MisoString
friendlyAuthError code = case (fromMisoString code :: String) of
  "email_not_confirmed" -> "Please check your email and confirm your account before signing in."
  "invalid_credentials" -> "Invalid email or password."
  "user_already_exists" -> "An account with this email already exists."
  _ -> code

homeURI :: URI
homeURI = emptyURI

signInURI :: URI
signInURI = emptyURI { uriPath = "sign-in" }

signUpURI :: URI
signUpURI = emptyURI { uriPath = "sign-up" }

configURI :: URI
configURI = emptyURI { uriPath = "new-game" }

playURI :: MisoString -> URI
playURI uuid = emptyURI { uriPath = "play/" <> uuid }

gamePermalinkURI :: MisoString -> URI
gamePermalinkURI uuid = emptyURI { uriPath = "games/" <> uuid }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
foreign import javascript unsafe "globalThis.playMoveSound()"
  js_playMoveSound :: IO ()
foreign import javascript unsafe "globalThis.getSupabaseSession($1,$2)"
  js_getSupabaseSession :: Function -> Function -> IO ()
foreign import javascript unsafe "globalThis.toggleTheme()"
  js_toggleDarkMode :: IO ()
foreign import javascript unsafe "globalThis.loadLocalGames($1,$2)"
  js_loadLocalGames :: Function -> Function -> IO ()
foreign import javascript unsafe "globalThis.clearLocalGames()"
  js_clearLocalGames :: IO ()
foreign import javascript unsafe "globalThis.generateUUID()"
  js_generateUUID_raw :: IO JSVal
foreign import javascript unsafe "globalThis.copyToClipboard($1)"
  js_copyToClipboard_raw :: JSVal -> IO ()
#else
js_playMoveSound :: IO ()
js_playMoveSound = pure ()
js_getSupabaseSession :: Function -> Function -> IO ()
js_getSupabaseSession _ _ = pure ()
js_toggleDarkMode :: IO ()
js_toggleDarkMode = pure ()
js_loadLocalGames :: Function -> Function -> IO ()
js_loadLocalGames _ _ = pure ()
js_clearLocalGames :: IO ()
js_clearLocalGames = pure ()
js_generateUUID_raw :: IO JSVal
js_generateUUID_raw = toJSVal ("00000000-0000-0000-0000-000000000000" :: MisoString)
js_copyToClipboard_raw :: JSVal -> IO ()
js_copyToClipboard_raw _ = pure ()
#endif

js_generateUUID :: IO MisoString
js_generateUUID = fromJSValUnchecked =<< js_generateUUID_raw

js_copyToClipboard :: MisoString -> IO ()
js_copyToClipboard s = toJSVal s >>= js_copyToClipboard_raw

-- | Save a game record to localStorage via the Miso DSL.
saveLocalGameIO :: Value -> IO ()
saveLocalGameIO gameData = do
  val <- toJSVal gameData
  void $ jsg "globalThis" # "saveLocalGame" $ val

main :: IO ()
main = startApp defaultEvents app
  where
    app = Component
      { model            = initModel
      , hydrateModel     = Nothing
      , update           = updateModel
      , view             = viewModel
      , subs             = [ uriSub HandleURI
                           , \sink -> getURI >>= sink . HandleURI
                           , \sink -> do
                               okCb <- successCallback sink
                                 (\_ -> SessionRestored Nothing)
                                 (SessionRestored . Just)
                               errCb <- errorCallback sink
                                 (\_ -> SessionRestored Nothing)
                               js_getSupabaseSession okCb errCb
                           ]
      , styles           = []
      , scripts          = []
      , mountPoint       = Nothing
      , logLevel         = Off
      , mailbox          = const Nothing
      , bindings         = []
      , eventPropagation = False
      , mount            = Nothing
      , unmount          = Nothing
      }

initModel :: Model
initModel = Model
  { mScreen         = HomeScreen
  , mGameMode       = AiMode
  , mGameState      = initialState Tablut
  , mSelected       = Nothing
  , mValidMoves     = []
  , mVariant        = Tablut
  , mAiSide         = DefenderSide
  , mAiThinking     = False
  , mAiDepth        = 4
  , mAiNodeLimit    = 10000
  , mHistory        = []
  , mSession        = Nothing
  , mAuthEmail      = ""
  , mAuthPassword   = ""
  , mAuthError      = Nothing
  , mAuthMessage    = Nothing
  , mAuthLoading    = False
  , mPastGames      = []
  , mGamesLoading   = False
  , mConfigExpanded = False
  , mShowQuoteRef  = False
  , mToast         = Nothing
  , mShowDepthInfo = False
  , mShowNodesInfo = False
  , mLocalGames    = []
  , mMoveList      = []
  , mReplayGame    = Nothing
  , mReplayStates  = []
  , mReplayIndex   = 0
  , mGameId        = Nothing
  , mIsPublic      = False
  , mReplayNotFound = False
  }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: Action -> Effect ROOT () Model Action
updateModel = \case
  NoOp -> pure ()

  SetGameMode mode ->
    modify $ \m -> m { mGameMode = mode }

  SetVariant variant ->
    modify $ \m -> m { mVariant = variant }

  StartGame ->
    withSink $ \sink -> do
      uuid <- js_generateUUID
      sink (InitGame uuid)

  GotoHome ->
    io_ $ pushURI homeURI

  GotoSignIn ->
    io_ $ pushURI signInURI

  GotoSignUp ->
    io_ $ pushURI signUpURI

  GotoConfig ->
    io_ $ pushURI configURI

  ToggleConfigExpand ->
    modify $ \m -> m { mConfigExpanded = not (mConfigExpanded m) }

  ToggleQuoteRef ->
    modify $ \m -> m { mShowQuoteRef = not (mShowQuoteRef m) }

  ShowToast msg -> do
    modify $ \m -> m { mToast = Just msg }
    withSink $ \sink -> do
      threadDelay 3000000
      sink DismissToast

  DismissToast ->
    modify $ \m -> m { mToast = Nothing }

  ToggleDepthInfo ->
    modify $ \m -> m { mShowDepthInfo = not (mShowDepthInfo m) }

  ToggleNodesInfo ->
    modify $ \m -> m { mShowNodesInfo = not (mShowNodesInfo m) }

  HandleURI uri -> do
    m <- get
    case parseRoute uri of
      PlayRoute uuid
        | mGameId m == Just uuid && mScreen m == GameScreen -> pure ()
        | otherwise -> io_ $ pushURI (gamePermalinkURI uuid)
      GameRoute uuid -> do
        modify $ \x -> x
          { mScreen = ReplayScreen
          , mReplayGame = Nothing
          , mReplayStates = []
          , mReplayIndex = 0
          , mReplayNotFound = False
          }
        selectWithFilters "games" "*"
          [eq "id" uuid]
          (FetchOptions Nothing Nothing)
          ReplayLoaded ReplayLoadError
      HomeRoute -> do
        modify $ \x -> x { mScreen = HomeScreen, mAiThinking = False, mGameId = Nothing }
        loadPastGames
      SignInRoute ->
        modify $ \x -> x { mScreen = SignInScreen, mAuthError = Nothing, mAuthMessage = Nothing }
      SignUpRoute ->
        modify $ \x -> x { mScreen = SignUpScreen, mAuthError = Nothing, mAuthMessage = Nothing }
      ConfigRoute ->
        modify $ \x -> x { mScreen = ConfigScreen, mMoveList = [], mGameId = Nothing }

  CellClicked coords -> do
    m <- get
    let gs    = mGameState m
        board = gsBoard gs
        side  = turnSide gs
        piece = pieceAt board coords
        aiBlocked = mGameMode m == AiMode && mAiSide m == side
    if finished (gsResult gs) || mAiThinking m || aiBlocked
      then pure ()
      else case mSelected m of
        Just sel | coords `elem` mValidMoves m -> do
          let move = MoveAction sel coords
              gs' = act gs move
          modify $ const $ m { mGameState = gs', mSelected = Nothing, mValidMoves = []
                             , mHistory = mHistory m ++ [gs]
                             , mMoveList = mMoveList m ++ [move] }
          io_ js_playMoveSound
          when (finished (gsResult gs')) saveGame
          triggerAi
        Just sel | sel == coords ->
          modify $ const $ m { mSelected = Nothing, mValidMoves = [] }
        _ | canControl side piece -> do
          let moves = getPossibleMovesFrom gs coords
          modify $ const $ m { mSelected = Just coords, mValidMoves = moves }
        _ ->
          modify $ const $ m { mSelected = Nothing, mValidMoves = [] }

  AiMoveComplete move -> do
    m <- get
    if mAiThinking m
      then do
        let gs = mGameState m
            gs' = act gs move
        modify $ const $ m
          { mGameState = gs', mSelected = Nothing
          , mValidMoves = [], mAiThinking = False
          , mHistory = mHistory m ++ [gs]
          , mMoveList = mMoveList m ++ [move] }
        io_ js_playMoveSound
        when (finished (gsResult gs')) saveGame
      else pure ()

  SetAiSide side ->
    modify $ \m -> m { mAiSide = side }

  SetAiDepth d ->
    modify $ \m -> m { mAiDepth = max 1 (min 8 d) }

  SetAiNodeLimit n ->
    modify $ \m -> m { mAiNodeLimit = n }

  GotoMove i -> do
    m <- get
    let allStates = mHistory m ++ [mGameState m]
        currentIdx = length allStates - 1
    if i >= 0 && i < currentIdx
      then put $ m
          { mGameState  = allStates !! i
          , mHistory    = take i allStates
          , mSelected   = Nothing
          , mValidMoves = []
          , mAiThinking = False
          }
      else pure ()

  Undo -> do
    m <- get
    case mHistory m of
      [] -> pure ()
      _  -> do
        let prev = last (mHistory m)
        put $ m
          { mGameState  = prev
          , mHistory    = init (mHistory m)
          , mSelected   = Nothing
          , mValidMoves = []
          , mAiThinking = False
          }

  -- Auth actions
  SetAuthEmail e ->
    modify $ \m -> m { mAuthEmail = e }

  SetAuthPassword p ->
    modify $ \m -> m { mAuthPassword = p }

  DoSignIn -> do
    m <- get
    modify $ \x -> x { mAuthLoading = True, mAuthError = Nothing, mAuthMessage = Nothing }
    let creds = SignInCredentials
          { sicEmail = Email (mAuthEmail m)
          , sicPassword = Password (mAuthPassword m)
          }
    signInWithPassword creds AuthSuccess AuthError

  DoSignUp -> do
    m <- get
    modify $ \x -> x { mAuthLoading = True, mAuthError = Nothing, mAuthMessage = Nothing }
    let signup = SignUpEmail
          { sueEmail = Email (mAuthEmail m)
          , suePassword = mAuthPassword m
          , sueOptions = Nothing
          }
    signUpEmail signup AuthSuccess AuthError

  AuthSuccess resp -> do
    case adSession (arData resp) of
      Just sess -> do
        modify $ \m -> m
          { mSession      = Just sess
          , mAuthEmail    = ""
          , mAuthPassword = ""
          , mAuthError    = Nothing
          , mAuthMessage  = Nothing
          , mAuthLoading  = False
          , mLocalGames   = []
          }
        migrateLocalGames sess
        io_ $ pushURI homeURI
      Nothing ->
        modify $ \m -> m
          { mAuthEmail    = ""
          , mAuthPassword = ""
          , mAuthError    = Nothing
          , mAuthMessage  = Just "Check your email to confirm your account."
          , mAuthLoading  = False
          }

  AuthError msg ->
    modify $ \m -> m { mAuthError = Just (friendlyAuthError msg), mAuthLoading = False }

  DoSignOut -> do
    signOut defaultSignOutOptions SignOutSuccess AuthError
    modify $ \m -> m
      { mSession    = Nothing
      , mPastGames  = []
      , mLocalGames = []
      , mAuthError  = Nothing
      }

  SignOutSuccess _ -> pure ()

  SessionRestored mSess -> do
    modify $ \m -> m { mSession = mSess }
    case mSess of
      Just _  -> loadPastGames
      Nothing -> pure ()

  GameSaved _ -> loadPastGames
  GameSaveError _ -> pure ()

  LocalGamesLoaded games ->
    modify $ \m -> m { mLocalGames = games }

  DoMigrateGames uid games -> do
    mapM_ (\gr -> do
      let gameData = object
            [ "user_id"     .= uid
            , "variant"     .= grVariant gr
            , "winner"      .= grWinner gr
            , "result_desc" .= grResultDesc gr
            , "total_moves" .= grTotalMoves gr
            , "game_mode"   .= grGameMode gr
            , "ai_side"     .= grAiSide gr
            , "ai_depth"    .= grAiDepth gr
            , "played_at"   .= grPlayedAt gr
            , "moves"       .= grMoves gr
            ]
      insert "games" gameData
        (InsertOptions Nothing Nothing)
        GameSaved GameSaveError
      ) games
    io_ js_clearLocalGames

  GamesLoaded val ->
    case fromJSON val of
      Success games -> modify $ \m -> m { mPastGames = games, mGamesLoading = False }
      Error _       -> modify $ \m -> m { mPastGames = [], mGamesLoading = False }

  GamesLoadError _ ->
    modify $ \m -> m { mGamesLoading = False }

  ToggleTheme ->
    io_ js_toggleDarkMode

  GotoReplay gid ->
    io_ $ pushURI (emptyURI { uriPath = "games/" <> gid })

  ReplayLoaded val ->
    case fromJSON val of
      Success games -> case (games :: [GameRecord]) of
        (gr:_) -> do
          case (lookupVariant (grVariant gr), grMoves gr) of
            (Just variant, Just moves) -> do
              let initial = initialState variant
                  states = scanl act initial moves
              modify $ \m -> m
                { mReplayGame   = Just gr
                , mReplayStates = states
                , mReplayIndex  = length states - 1
                }
            _ -> modify $ \m -> m
              { mReplayGame   = Just gr
              , mReplayStates = []
              , mReplayIndex  = 0
              }
        [] -> modify $ \m -> m { mReplayNotFound = True }
      Error _ -> modify $ \m -> m { mReplayNotFound = True }

  ReplayLoadError _ -> pure ()

  ReplayGotoMove i -> do
    m <- get
    let maxIdx = length (mReplayStates m) - 1
    modify $ \x -> x { mReplayIndex = max 0 (min maxIdx i) }

  InitGame uuid -> do
    m <- get
    let gs = initialState (mVariant m)
    put $ m
      { mGameId     = Just uuid
      , mIsPublic   = False
      , mScreen     = GameScreen
      , mGameState  = gs
      , mSelected   = Nothing
      , mValidMoves = []
      , mAiThinking = False
      , mHistory    = []
      , mMoveList   = []
      }
    case mSession m of
      Just sess -> do
        let uid = userId (sessionUser sess)
            gameModeStr' = case mGameMode m of
              LocalMode -> "local" :: MisoString
              AiMode    -> "ai"
            aiSideStr' = if mGameMode m == AiMode
              then Just (case mAiSide m of
                AttackerSide -> "attacker" :: MisoString
                DefenderSide -> "defender")
              else Nothing
            aiDepthVal' = if mGameMode m == AiMode
              then Just (mAiDepth m)
              else (Nothing :: Maybe Int)
            gameData = object
              [ "id"          .= uuid
              , "user_id"     .= uid
              , "variant"     .= variantSlug (mVariant m)
              , "result_desc" .= ("in_progress" :: MisoString)
              , "total_moves" .= (0 :: Int)
              , "game_mode"   .= gameModeStr'
              , "ai_side"     .= aiSideStr'
              , "ai_depth"    .= aiDepthVal'
              , "moves"       .= ([] :: [MoveAction])
              ]
        insert "games" gameData
          (InsertOptions Nothing Nothing)
          GameCreated GameCreateError
      Nothing -> pure ()
    io_ $ pushURI (playURI uuid)
    triggerAi

  GameCreated _ -> pure ()

  GameCreateError msg ->
    modify $ \m -> m { mToast = Just ("Failed to create game: " <> msg) }

  ToggleGamePublic -> do
    m <- get
    case (mSession m, mGameId m) of
      (Just _, Just gid) -> do
        let newVal = not (mIsPublic m)
        modify $ \x -> x { mIsPublic = newVal, mToast = Just (if newVal then "Game is now public" else "Game is now private") }
        updateTable "games"
          (object ["is_public" .= newVal])
          [eq "id" gid]
          (UpdateOptions Nothing)
          GameUpdated GameUpdateError
        withSink $ \sink -> do
          threadDelay 3000000
          sink DismissToast
      _ -> pure ()

  GameUpdated _ -> pure ()
  GameUpdateError _ -> pure ()

  CopyGameLink -> do
    m <- get
    case mGameId m of
      Just gid -> do
        io_ $ js_copyToClipboard ("https://taflhouse.com/games/" <> gid)
        modify $ \x -> x { mToast = Just "Link copied!" }
        withSink $ \sink -> do
          threadDelay 3000000
          sink DismissToast
      Nothing -> pure ()

-- | Check if the AI should move and trigger the search if so.
triggerAi :: Effect ROOT () Model Action
triggerAi = do
  m <- get
  let gs = mGameState m
  if mGameMode m == AiMode && not (finished (gsResult gs)) && mAiSide m == turnSide gs
    then do
      modify $ \x -> x { mAiThinking = True }
      let cfg = AiConfig (mAiDepth m) (mAiNodeLimit m)
      withSink $ \sink -> do
        threadDelay 100000
        case bestMove cfg gs of
          Nothing   -> sink NoOp
          Just move -> sink (AiMoveComplete move)
    else pure ()

-- | Save the current finished game (Supabase update if authenticated with gameId, localStorage if guest).
saveGame :: Effect ROOT () Model Action
saveGame = do
  m <- get
  let gs = mGameState m
      result = gsResult gs
      winnerStr = fmap (\s -> case s of
        AttackerSide -> "attacker" :: MisoString
        DefenderSide -> "defender") (winner result)
      gameModeStr = case mGameMode m of
        LocalMode -> "local" :: MisoString
        AiMode    -> "ai"
      aiSideStr = if mGameMode m == AiMode
        then Just (case mAiSide m of
          AttackerSide -> "attacker" :: MisoString
          DefenderSide -> "defender")
        else Nothing
      aiDepthVal = if mGameMode m == AiMode
        then Just (mAiDepth m)
        else (Nothing :: Maybe Int)
  case (mSession m, mGameId m) of
    (Just _, Just gid) -> do
      let updateData = object
            [ "result_desc" .= ms (desc result)
            , "winner"      .= winnerStr
            , "total_moves" .= gsTurn gs
            , "moves"       .= mMoveList m
            ]
      updateTable "games" updateData
        [eq "id" gid]
        (UpdateOptions Nothing)
        GameUpdated GameUpdateError
    _ -> do
      let gameData = object
            [ "variant"     .= variantSlug (mVariant m)
            , "winner"      .= winnerStr
            , "result_desc" .= ms (desc result)
            , "total_moves" .= gsTurn gs
            , "game_mode"   .= gameModeStr
            , "ai_side"     .= aiSideStr
            , "ai_depth"    .= aiDepthVal
            , "moves"       .= mMoveList m
            ]
      io_ $ saveLocalGameIO gameData

-- | Load past games (Supabase if authenticated, localStorage if guest).
loadPastGames :: Effect ROOT () Model Action
loadPastGames = do
  m <- get
  case mSession m of
    Nothing ->
      withSink $ \sink -> do
        okCb <- successCallback sink
          (\_ -> LocalGamesLoaded [])
          LocalGamesLoaded
        errCb <- errorCallback sink
          (\_ -> LocalGamesLoaded [])
        js_loadLocalGames okCb errCb
    Just sess -> do
      modify $ \x -> x { mGamesLoading = True }
      let uid = userId (sessionUser sess)
      selectWithFilters "games" "*"
        [eq "user_id" uid]
        (FetchOptions Nothing Nothing)
        GamesLoaded GamesLoadError

-- | Migrate local games to Supabase on auth success.
migrateLocalGames :: Session -> Effect ROOT () Model Action
migrateLocalGames sess =
  withSink $ \sink -> do
    okCb <- successCallback sink
      (\_ -> NoOp)
      (\games -> if null (games :: [GameRecord]) then NoOp
                 else DoMigrateGames (userId (sessionUser sess)) games)
    errCb <- errorCallback sink
      (\_ -> NoOp)
    js_loadLocalGames okCb errCb

-- ---------------------------------------------------------------------------
-- View: Top-level layout
-- ---------------------------------------------------------------------------

viewModel :: () -> Model -> View Model Action
viewModel _ m =
  H.div_
    [ HP.class_ "fixed inset-0 flex flex-col bg-background font-sans"
    ]
    [ viewNavbar m
    , H.div_
        [ HP.class_ (if mScreen m == HomeScreen then "flex-1" else "flex-1 overflow-y-auto overscroll-none")
        ]
        [ H.div_
            [ HP.class_ "flex flex-col items-center min-h-full pt-8 pb-12 px-4 mx-auto w-full max-w-7xl"
            ]
            [ case mScreen m of
                HomeScreen   -> viewHome m
                SignInScreen -> viewSignIn m
                SignUpScreen -> viewSignUp m
                ConfigScreen -> viewConfig m
                GameScreen   -> viewGame m
                ReplayScreen -> viewReplay m
            ]
        ]
    , viewToast m
    ]

viewToast :: Model -> View Model Action
viewToast m = case mToast m of
  Nothing -> text ""
  Just msg ->
    H.div_
      [ HP.class_ "fixed bottom-6 left-1/2 card px-4 py-2 text-sm text-foreground shadow-lg"
      , style_ [("transform", "translateX(-50%)"), ("z-index", "100")]
      , SVG.onClick DismissToast
      ]
      [ text msg ]

-- ---------------------------------------------------------------------------
-- Navbar
-- ---------------------------------------------------------------------------

viewNavbar :: Model -> View Model Action
viewNavbar m =
  H.div_
    [ HP.class_ "border-b border-border bg-background/95 backdrop-blur shrink-0"
    ]
    [ H.div_
        [ HP.class_ "flex items-center justify-between px-4 py-3 mx-auto w-full max-w-7xl"
        ]
        [ -- Left: brand
          H.span_
            [ HP.class_ "text-xl font-bold tracking-widest text-foreground/80 cursor-pointer select-none"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GotoHome
            ]
            [ text "TAFLHOUSE" ]
        , -- Right: controls
          H.div_
            [ HP.class_ "flex items-center gap-4"
            ]
            (themeToggleBtn : navAuthButtons m)
        ]
    ]

themeToggleBtn :: View Model Action
themeToggleBtn =
  H.button_
    [ HP.class_ "p-2 rounded-md text-foreground hover:bg-muted cursor-pointer"
    , style_ [("touch-action", "manipulation"), ("background", "none"), ("border", "none")]
    , SVG.onClick ToggleTheme
    , HP.title_ "Toggle theme"
    ]
    [ iconSun, iconMoon ]

iconSun :: View Model Action
iconSun =
  SVG.svg_
    [ HP.class_ "hidden dark:block"
    , SP.viewBox_ "0 0 24 24"
    , HP.width_ "18"
    , HP.height_ "18"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.circle_ [ SP.cx_ "12", SP.cy_ "12", SP.r_ "4" ]
    , SVG.path_ [ SP.d_ "M12 2v2" ]
    , SVG.path_ [ SP.d_ "M12 20v2" ]
    , SVG.path_ [ SP.d_ "m4.93 4.93 1.41 1.41" ]
    , SVG.path_ [ SP.d_ "m17.66 17.66 1.41 1.41" ]
    , SVG.path_ [ SP.d_ "M2 12h2" ]
    , SVG.path_ [ SP.d_ "M20 12h2" ]
    , SVG.path_ [ SP.d_ "m6.34 17.66-1.41 1.41" ]
    , SVG.path_ [ SP.d_ "m19.07 4.93-1.41 1.41" ]
    ]

iconMoon :: View Model Action
iconMoon =
  SVG.svg_
    [ HP.class_ "dark:hidden"
    , SP.viewBox_ "0 0 24 24"
    , HP.width_ "18"
    , HP.height_ "18"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z" ] ]

navAuthButtons :: Model -> [View Model Action]
navAuthButtons m =
    (case mSession m of
      Just _ ->
        [ H.span_
            [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick DoSignOut
            ]
            [ SVG.svg_
                [ HP.class_ "sm:hidden"
                , SP.viewBox_ "0 0 24 24"
                , HP.width_ "18"
                , HP.height_ "18"
                , SP.fill_ "none"
                , SP.stroke_ "currentcolor"
                , SP.strokeWidth_ "2"
                , SP.strokeLinecap_ "round"
                , SP.strokeLinejoin_ "round"
                ]
                [ SVG.path_ [ SP.d_ "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4" ]
                , SVG.polyline_ [ SP.points_ "16 17 21 12 16 7" ]
                , SVG.line_ [ SP.x1_ "21", SP.y1_ "12", SP.x2_ "9", SP.y2_ "12" ]
                ]
            , H.span_
                [ HP.class_ "hidden sm:inline" ]
                [ text "Logout" ]
            ]
        ]
      Nothing ->
        [ H.span_
            [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GotoSignIn
            ]
            [ text "Sign In" ]
        ]
    ) ++
    case mSession m of
      Just _ | null (mPastGames m), not (mGamesLoading m) -> []
      _ ->
        [ H.button_
            [ HP.class_ "btn btn-sm"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GotoConfig
            ]
            [ text "New Game" ]
        ]

-- ---------------------------------------------------------------------------
-- Home Screen
-- ---------------------------------------------------------------------------

viewHome :: Model -> View Model Action
viewHome m = case mSession m of
  Just _ | mGamesLoading m ->
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8 animate-pulse"
      ]
      [ text "Loading games..." ]

  Just _ | not (null (mPastGames m)) ->
    H.div_
      [ HP.class_ "w-full max-w-2xl"
      , style_ [("margin-top", "4em")]
      ]
      [ H.div_
          [ HP.class_ "flex justify-between items-center mb-4"
          ]
          [ H.h2_
              [ HP.class_ "text-lg font-semibold text-foreground"
              ]
              [ text "Your Games" ]
          ]
      , viewPastGamesTable (mPastGames m)
      ]

  Just _ ->
    H.div_
      [ HP.class_ "text-center max-w-md"
      , style_ [("margin-top", "4em")]
      ]
      [ H.p_
          [ HP.class_ "text-xl font-bold"
          ]
          [ text "No games yet" ]
      , H.button_
          [ HP.class_ "btn"
          , style_ [("touch-action", "manipulation"), ("margin-top", "2em")]
          , SVG.onClick GotoConfig
          ]
          [ text "New Game" ]
      , H.p_
          [ HP.class_ "text-muted-foreground text-sm italic cursor-pointer"
          , style_ [("margin-top", "2em"), ("text-decoration", "underline dotted"), ("text-underline-offset", "4px")]
          , SVG.onClick (ShowToast "V\x01EBlusp\x00E1, stanza 59")
          ]
          [ text "\"The golden tafl pieces shall again be found in the grass.\"" ]
      ]

  Nothing ->
    H.div_
      [ HP.class_ "text-center max-w-md"
      , style_ [("margin-top", "4em")]
      ]
      [ H.p_
          [ HP.class_ "text-xl font-bold"
          ]
          [ text "Welcome! Feel free to settle in." ]
      , H.button_
          [ HP.class_ "btn"
          , style_ [("touch-action", "manipulation"), ("margin-top", "2em")]
          , SVG.onClick GotoConfig
          ]
          [ text "New Game" ]
      , H.div_
          [ style_ [("margin-top", "2em"), ("position", "relative")]
          ]
          [ H.p_
              [ HP.class_ "text-muted-foreground text-sm italic"
              , style_ [("text-decoration", "underline dotted"), ("text-underline-offset", "4px"), ("cursor", "pointer")]
              , SVG.onClick ToggleQuoteRef
              ]
              [ text "\"They played tafl in the meadow and were merry,\"" ]
          , if mShowQuoteRef m
              then H.div_
                [ HP.class_ "card p-4 text-left"
                , style_ [ ("position", "absolute"), ("top", "100%"), ("left", "50%")
                         , ("transform", "translateX(-50%)"), ("margin-top", "0.5em")
                         , ("width", "18rem"), ("z-index", "50")
                         ]
                ]
                [ H.p_
                    [ HP.class_ "text-sm text-muted-foreground" ]
                    [ text "V\x01EBlusp\x00E1, stanza 8" ]
                ]
              else text ""
          ]
      ]

-- ---------------------------------------------------------------------------
-- Past Games Table
-- ---------------------------------------------------------------------------

viewPastGamesTable :: [GameRecord] -> View Model Action
viewPastGamesTable games =
  H.div_
    [ HP.class_ "overflow-x-auto"
    ]
    [ H.table_
        [ HP.class_ "table w-full"
        ]
        [ H.thead_
            []
            [ H.tr_
                []
                [ H.th_ [] [ text "Variant" ]
                , H.th_ [] [ text "Mode" ]
                , H.th_ [] [ text "Result" ]
                , H.th_ [] [ text "Moves" ]
                , H.th_ [] [ text "Date" ]
                ]
            ]
        , H.tbody_
            []
            (map viewGameRow games)
        ]
    ]

viewGameRow :: GameRecord -> View Model Action
viewGameRow gr =
  let winText = case grWinner gr of
        Just "attacker" -> "Attackers won"
        Just "defender" -> "Defenders won"
        _               -> "Draw"
      modeText = case grGameMode gr of
        "ai"    -> "vs AI"
        "local" -> "Local"
        _       -> grGameMode gr
      cells =
        [ H.td_ [] [ text (grVariant gr) ]
        , H.td_ [] [ text modeText ]
        , H.td_ [] [ text winText ]
        , H.td_ [] [ text (ms (show (grTotalMoves gr))) ]
        , H.td_ [ HP.class_ "text-muted-foreground" ] [ text (grPlayedAt gr) ]
        ]
  in case grId gr of
    Just gid -> H.tr_
      [ HP.class_ "cursor-pointer hover:bg-muted/50"
      , SVG.onClick (GotoReplay gid)
      ] cells
    Nothing -> H.tr_ [] cells

-- ---------------------------------------------------------------------------
-- Sign In Screen
-- ---------------------------------------------------------------------------

viewSignIn :: Model -> View Model Action
viewSignIn m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "Sign In" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit DoSignIn
            ]
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "email"
                , HP.required_ True
                , HP.value_ (mAuthEmail m)
                , HP.placeholder_ "Email"
                , H.onInput SetAuthEmail
                ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "password"
                , HP.required_ True
                , HP.value_ (mAuthPassword m)
                , HP.placeholder_ "Password"
                , H.onInput SetAuthPassword
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm"
                  ]
                  [ text err ]
            , case mAuthMessage m of
                Nothing  -> H.div_ [] []
                Just msg -> H.div_
                  [ HP.class_ "text-emerald-600 dark:text-emerald-400 text-sm"
                  ]
                  [ text msg ]
            , if mAuthLoading m
                then H.div_
                  [ HP.class_ "text-center text-muted-foreground text-sm"
                  ]
                  [ text "Loading..." ]
                else H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  ]
                  [ text "Sign In" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground"
            ]
            [ text "Don't have an account? "
            , H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoSignUp
                ]
                [ text "Sign Up" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Sign Up Screen
-- ---------------------------------------------------------------------------

viewSignUp :: Model -> View Model Action
viewSignUp m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "Sign Up" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit DoSignUp
            ]
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "email"
                , HP.required_ True
                , HP.value_ (mAuthEmail m)
                , HP.placeholder_ "Email"
                , H.onInput SetAuthEmail
                ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "password"
                , HP.required_ True
                , HP.value_ (mAuthPassword m)
                , HP.placeholder_ "Password"
                , H.onInput SetAuthPassword
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm"
                  ]
                  [ text err ]
            , case mAuthMessage m of
                Nothing  -> H.div_ [] []
                Just msg -> H.div_
                  [ HP.class_ "text-emerald-600 dark:text-emerald-400 text-sm"
                  ]
                  [ text msg ]
            , if mAuthLoading m
                then H.div_
                  [ HP.class_ "text-center text-muted-foreground text-sm"
                  ]
                  [ text "Loading..." ]
                else H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  ]
                  [ text "Sign Up" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground"
            ]
            [ text "Already have an account? "
            , H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoSignIn
                ]
                [ text "Sign In" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Config Screen
-- ---------------------------------------------------------------------------

viewConfig :: Model -> View Model Action
viewConfig m =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-md"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "New Game" ]
        , viewConfigSummary m
        , H.div_
            [ HP.class_ "mt-4 flex flex-col items-center gap-2"
            ]
            [ H.button_
                [ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick StartGame
                ]
                [ text "Start Game" ]
            , H.span_
                [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick ToggleConfigExpand
                ]
                [ text (if mConfigExpanded m then "Hide options" else "Configure") ]
            ]
        , if mConfigExpanded m
            then viewConfigOptions m
            else H.div_ [] []
        ]
    ]

viewConfigSummary :: Model -> View Model Action
viewConfigSummary m =
  let modeText = case mGameMode m of
        LocalMode -> "Local (2 players)"
        AiMode    -> "vs AI"
      boardText = variantName (mVariant m)
      aiInfo = if mGameMode m == AiMode
        then [ ("AI plays" :: MisoString, if mAiSide m == AttackerSide then "Attackers" else "Defenders")
             ]
        else []
      items = [("Mode", modeText), ("Board", boardText)] ++ aiInfo
  in H.div_
    [ HP.class_ "flex flex-col gap-1"
    ]
    (map summaryRow items)

summaryRow :: (MisoString, MisoString) -> View Model Action
summaryRow (label, val) =
  H.div_
    [ HP.class_ "flex justify-between py-1 border-b border-border text-sm"
    ]
    [ H.span_
        [ HP.class_ "text-muted-foreground"
        ]
        [ text label ]
    , H.span_
        [ HP.class_ "font-medium"
        ]
        [ text val ]
    ]

viewConfigOptions :: Model -> View Model Action
viewConfigOptions m =
  H.div_
    [ HP.class_ "mt-4 pt-4 border-t border-border flex flex-col gap-4"
    ]
    [ setupSection "Mode"
        [ setupBtn (SetGameMode LocalMode) "Local" (mGameMode m == LocalMode)
        , setupBtn (SetGameMode AiMode) "vs AI" (mGameMode m == AiMode)
        , setupBtnDisabled "Multiplayer (coming soon)"
        ]
    , setupSection "Board"
        [ setupBtn (SetVariant Brandubh) "Brandubh 7x7" (mVariant m == Brandubh)
        , setupBtn (SetVariant Tablut) "Tablut 9x9" (mVariant m == Tablut)
        , setupBtn (SetVariant Classic) "Copenhagen 11x11" (mVariant m == Classic)
        , setupBtn (SetVariant Line) "Line 11x11" (mVariant m == Line)
        , setupBtn (SetVariant Tawlbwrdd) "Tawlbwrdd 11x11" (mVariant m == Tawlbwrdd)
        ]
    , if mGameMode m == AiMode then viewSetupAi m else H.div_ [] []
    ]

-- ---------------------------------------------------------------------------
-- Setup helpers (shared by config options)
-- ---------------------------------------------------------------------------

setupSection :: MisoString -> [View Model Action] -> View Model Action
setupSection label children =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4"
        ]
        [ text label ]
    , H.div_
        [ HP.class_ "flex gap-2 flex-wrap justify-center"
        ]
        children
    ]

setupBtn :: Action -> MisoString -> Bool -> View Model Action
setupBtn action label isActive =
  H.button_
    [ HP.class_ (if isActive then "btn btn-secondary" else "btn btn-outline text-foreground")
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

setupBtnDisabled :: MisoString -> View Model Action
setupBtnDisabled label =
  H.button_
    [ HP.class_ "btn btn-outline text-muted-foreground opacity-60 cursor-not-allowed"
    , HP.disabled_
    ]
    [ text label ]

viewSetupAi :: Model -> View Model Action
viewSetupAi m =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4"
        ]
        [ text "AI SETTINGS" ]
    -- AI side
    , H.div_
        [ HP.class_ "flex gap-2 flex-wrap justify-center mb-2"
        ]
        [ setupBtn (SetAiSide AttackerSide) "AI plays Attackers" (mAiSide m == AttackerSide)
        , setupBtn (SetAiSide DefenderSide) "AI plays Defenders" (mAiSide m == DefenderSide)
        ]
    -- Depth
    , H.div_
        [ HP.class_ "text-center", style_ [("position", "relative")] ]
        [ H.div_
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4 flex items-center justify-center gap-1" ]
            [ text "DEPTH"
            , H.span_
                [ HP.class_ "inline-flex items-center justify-center w-4 h-4 rounded-full border border-muted-foreground text-[10px] cursor-pointer"
                , SVG.onClick ToggleDepthInfo
                ]
                [ text "?" ]
            ]
        , if mShowDepthInfo m
            then H.div_
              [ HP.class_ "card p-3 text-left text-sm text-muted-foreground"
              , style_ [ ("position", "absolute"), ("top", "1.5em"), ("left", "50%")
                       , ("transform", "translateX(-50%)"), ("width", "16rem"), ("z-index", "50")
                       ]
              ]
              [ text "How many moves ahead the AI looks. Higher = stronger but slower." ]
            else text ""
        , H.div_
            [ HP.class_ "flex gap-1 flex-wrap justify-center" ]
            [ setupBtn (SetAiDepth d) (ms (show d)) (mAiDepth m == d)
            | d <- [1..8]
            ]
        ]
    -- Node limit
    , H.div_
        [ HP.class_ "text-center", style_ [("position", "relative")] ]
        [ H.div_
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4 flex items-center justify-center gap-1" ]
            [ text "NODES"
            , H.span_
                [ HP.class_ "inline-flex items-center justify-center w-4 h-4 rounded-full border border-muted-foreground text-[10px] cursor-pointer"
                , SVG.onClick ToggleNodesInfo
                ]
                [ text "?" ]
            ]
        , if mShowNodesInfo m
            then H.div_
              [ HP.class_ "card p-3 text-left text-sm text-muted-foreground"
              , style_ [ ("position", "absolute"), ("top", "1.5em"), ("left", "50%")
                       , ("transform", "translateX(-50%)"), ("width", "16rem"), ("z-index", "50")
                       ]
              ]
              [ text "Max positions the AI evaluates per move. Caps search time. \x2018None\x2019 = unlimited." ]
            else text ""
        , H.div_
            [ HP.class_ "flex gap-1 flex-wrap justify-center" ]
            [ setupBtn (SetAiNodeLimit n) label (mAiNodeLimit m == n)
            | (n, label) <- [ (1000, "1K"), (5000, "5K")
                            , (10000, "10K"), (50000, "50K"), (100000, "100K")
                            , (0, "None")
                            ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Game Screen
-- ---------------------------------------------------------------------------

viewGame :: Model -> View Model Action
viewGame m =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center"
    ]
    [ viewBoardPanel m
    , viewStatus m
    , viewMoveHistory m
    ]

-- ---------------------------------------------------------------------------
-- Replay Screen
-- ---------------------------------------------------------------------------

viewReplay :: Model -> View Model Action
viewReplay m
  | mReplayNotFound m =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8"
      ]
      [ text "This game is private or doesn\x2019t exist." ]
  | Nothing <- mReplayGame m =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8 animate-pulse"
      ]
      [ text "Loading game..." ]
  | Just gr <- mReplayGame m =
    H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ viewReplayHeader gr
      , case mReplayStates m of
          [] -> H.div_
            [ HP.class_ "card p-6 text-center mt-4"
            ]
            [ text "No move data available for this game." ]
          states ->
            let gs = states !! mReplayIndex m
                n = boardSize (gsBoard gs)
            in H.div_
              [ HP.class_ "w-full flex flex-col items-center"
              ]
              [ viewReplayBoardPanel gs
              , viewReplayControls m n
              , viewReplayMoveList m n
              ]
      ]

viewReplayHeader :: GameRecord -> View Model Action
viewReplayHeader gr =
  let winText = case grWinner gr of
        Just "attacker" -> "Attackers won"
        Just "defender" -> "Defenders won"
        _               -> "Draw"
  in H.div_
    [ HP.class_ "text-center mb-2"
    , style_ [("margin-top", "2em")]
    ]
    [ H.h2_
        [ HP.class_ "text-lg font-bold" ]
        [ text (grVariant gr) ]
    , H.p_
        [ HP.class_ "text-sm text-muted-foreground" ]
        [ text (winText <> " \x00B7 " <> ms (show (grTotalMoves gr)) <> " moves") ]
    ]

viewReplayBoardPanel :: GameState -> View Model Action
viewReplayBoardPanel gs =
  let n = boardSize (gsBoard gs)
      totalPx = sqSize * n
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border w-full"
    , style_ [("max-width", ms totalPx <> "px"), ("margin-top", "1em")]
    ]
    [ viewReplaySVGBoard gs ]

viewReplaySVGBoard :: GameState -> View Model Action
viewReplaySVGBoard gs =
  let board = gsBoard gs
      n     = boardSize board
      total = sqSize * n
  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , HP.class_ "block aspect-square"
    ]
    ( svgDefs
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    ++ renderSpecialSquares gs n
    ++ [ renderPiece n r c (pieceAt board (Coords r c))
       | r <- [0..n-1], c <- [0..n-1]
       , pieceAt board (Coords r c) /= Empty
       ]
    ++ renderReplayLastMove gs n
    )

renderReplayLastMove :: GameState -> Int -> [View Model Action]
renderReplayLastMove gs _n = case gsLastAction gs of
  Nothing -> []
  Just (MoveAction f t) ->
    [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(200,200,80,0.3)"
        ]
    | sq <- [f, t]
    ]

viewReplayControls :: Model -> Int -> View Model Action
viewReplayControls m n =
  let idx = mReplayIndex m
      maxIdx = length (mReplayStates m) - 1
  in H.div_
    [ HP.class_ "flex items-center justify-center gap-2 my-4 w-full"
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ replayBtn (ReplayGotoMove 0) "|<" (idx > 0)
    , replayBtn (ReplayGotoMove (idx - 1)) "<" (idx > 0)
    , H.span_
        [ HP.class_ "text-sm font-mono text-muted-foreground min-w-[5em] text-center" ]
        [ text (ms (show idx) <> " / " <> ms (show maxIdx)) ]
    , replayBtn (ReplayGotoMove (idx + 1)) ">" (idx < maxIdx)
    , replayBtn (ReplayGotoMove maxIdx) ">|" (idx < maxIdx)
    ]

replayBtn :: Action -> MisoString -> Bool -> View Model Action
replayBtn action label enabled =
  H.button_
    [ HP.class_ (if enabled
        then "btn btn-outline btn-sm text-foreground"
        else "btn btn-outline btn-sm text-muted-foreground opacity-50 cursor-not-allowed")
    , style_ [("touch-action", "manipulation"), ("min-width", "2.5em")]
    , SVG.onClick (if enabled then action else NoOp)
    ]
    [ text label ]

viewReplayMoveList :: Model -> Int -> View Model Action
viewReplayMoveList m n =
  case grMoves =<< mReplayGame m of
    Nothing -> H.div_ [] []
    Just moves | null moves -> H.div_ [] []
    Just moves ->
      let states = mReplayStates m
          idx = mReplayIndex m
      in H.div_
        [ HP.class_ "flex flex-col gap-1 items-center w-full"
        , style_ [("max-width", ms (sqSize * n) <> "px")]
        ]
        [ H.div_
            [ HP.class_ "flex justify-between items-center w-full"
            , style_ [("margin-bottom", "0.4em")]
            ]
            [ H.span_
                [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase" ]
                [ text "MOVES" ]
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ replayMoveBtn i move n (i == idx) states
            | (i, move) <- reverse (zip [1..] moves)
            ]
        ]

replayMoveBtn :: Int -> MoveAction -> Int -> Bool -> [GameState] -> View Model Action
replayMoveBtn idx (MoveAction f t) n isCurrent states =
  let gs = if idx < length states then states !! idx else states !! (length states - 1)
      moveSide = opponentSide gs
      pointer = if isCurrent then "> " else "  "
      sideChar = case moveSide of
        AttackerSide -> "A"
        DefenderSide -> "D"
      label = pointer <> ms (show idx) <> ". " <> sideChar <> " "
            <> ms (coordStr n f) <> "-" <> ms (coordStr n t)
      activeCls = if isCurrent
        then " text-green-500 dark:text-green-400 border-l-2 border-green-500 dark:border-green-400"
        else " border-l-2 border-transparent"
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted/50 cursor-pointer bg-transparent border-0 text-foreground" <> activeCls)
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (ReplayGotoMove idx)
    ]
    [ text label ]

-- ---------------------------------------------------------------------------
-- Board
-- ---------------------------------------------------------------------------

sqSize :: Int
sqSize = 54

viewBoardPanel :: Model -> View Model Action
viewBoardPanel m =
  let n = boardSize (gsBoard (mGameState m))
      totalPx = sqSize * n
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border w-full"
    , style_ [("max-width", ms totalPx <> "px"), ("margin-top", "4em")]
    ]
    [ viewSVGBoard m ]

viewSVGBoard :: Model -> View Model Action
viewSVGBoard m =
  let gs    = mGameState m
      board = gsBoard gs
      n     = boardSize board
      total = sqSize * n
  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , HP.class_ "block aspect-square"
    ]
    ( svgDefs
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    ++ renderSpecialSquares gs n
    ++ renderHighlights m n
    ++ renderValidDots m n
    ++ [ renderPiece n r c (pieceAt board (Coords r c))
       | r <- [0..n-1], c <- [0..n-1]
       , pieceAt board (Coords r c) /= Empty
       ]
    ++ renderLastMove m n
    ++ [ renderClickTarget m n r c | r <- [0..n-1], c <- [0..n-1] ]
    )

svgDefs :: View Model Action
svgDefs =
  SVG.defs_ []
    [ SVG.filter_
        [ HP.id_ "pieceShadow"
        , SP.x_ "-20%", SP.y_ "-20%"
        , HP.width_ "140%", HP.height_ "160%"
        ]
        [ SVG.feDropShadow_
            [ SP.dx_ "0.3", SP.dy_ "0.7"
            , SP.stdDeviation_ "0.5"
            , SP.floodColor_ "#000000"
            , SP.floodOpacity_ "0.45"
            ]
        ]
    ]

-- Square background colors (themed via CSS variables)
renderSquareBg :: Int -> Int -> Int -> View Model Action
renderSquareBg _n r c =
  SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , style_ [("fill", if even (r + c) then "var(--muted)" else "var(--accent)")]
    ]

-- Mark corners and center (throne)
renderSpecialSquares :: GameState -> Int -> [View Model Action]
renderSpecialSquares gs n =
  let center = n `div` 2
      w      = cornerBaseWidth (gsRules gs)
      corners = [ (r, c)
                | r <- concatMap (\ww -> [ww, n - 1 - ww]) [0..w-1]
                , c <- concatMap (\ww -> [ww, n - 1 - ww]) [0..w-1]
                ]
      markSquare (r, c) color =
        SVG.rect_
          [ SP.x_ (ms (c * sqSize + 2))
          , SP.y_ (ms (r * sqSize + 2))
          , HP.width_ (ms (sqSize - 4))
          , HP.height_ (ms (sqSize - 4))
          , SP.fill_ "none"
          , SP.stroke_ color
          , SP.strokeWidth_ "2"
          , SP.rx_ "3"
          ]
  in map (\pos -> markSquare pos "var(--destructive)") corners
     ++ [markSquare (center, center) "var(--primary)"]

-- Highlight selected square
renderHighlights :: Model -> Int -> [View Model Action]
renderHighlights m _n = case mSelected m of
  Nothing -> []
  Just (Coords r c) ->
    [ SVG.rect_
        [ SP.x_ (ms (c * sqSize))
        , SP.y_ (ms (r * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(80,200,120,0.45)"
        ]
    ]

-- Valid move dots
renderValidDots :: Model -> Int -> [View Model Action]
renderValidDots m _n =
  [ SVG.circle_
      [ SP.cx_ (ms (col coord * sqSize + sqSize `div` 2))
      , SP.cy_ (ms (row coord * sqSize + sqSize `div` 2))
      , SP.r_ (ms (sqSize `div` 5))
      , SP.fill_ "rgba(80,200,120,0.6)"
      ]
  | coord <- mValidMoves m
  ]

-- Last move indicators
renderLastMove :: Model -> Int -> [View Model Action]
renderLastMove m _n = case gsLastAction (mGameState m) of
  Nothing -> []
  Just (MoveAction f t) ->
    [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(200,200,80,0.3)"
        ]
    | sq <- [f, t]
    ]

-- Piece rendering
renderPiece :: Int -> Int -> Int -> Piece -> View Model Action
renderPiece _n r c piece =
  let cx = c * sqSize + sqSize `div` 2
      cy = r * sqSize + sqSize `div` 2
      radius = sqSize `div` 2 - 4
      (fill, stroke, label) = case piece of
        Attacker -> ("#2a2a2a", "#111", "A" :: MisoString)
        Defender -> ("#e8e0d0", "#555", "D")
        King     -> ("#ffd700", "#8b6914", "K")
        Empty    -> ("#000", "#000", "")
  in SVG.g_
    [ SP.filter_ "url(#pieceShadow)" ]
    [ SVG.circle_
        [ SP.cx_ (ms cx)
        , SP.cy_ (ms cy)
        , SP.r_ (ms radius)
        , SP.fill_ fill
        , SP.stroke_ stroke
        , SP.strokeWidth_ "2"
        ]
    , SVG.text_
        [ SP.x_ (ms cx)
        , SP.y_ (ms (cy + 1))
        , SP.textAnchor_ "middle"
        , SP.dominantBaseline_ "central"
        , SP.fontSize_ (ms (sqSize `div` 3))
        , SP.fontWeight_ "bold"
        , SP.fill_ (if piece == Attacker then "#ccc" else "#333")
        , SP.fontFamily_ "Arial, sans-serif"
        ]
        [ text label ]
    ]

-- Transparent click targets
renderClickTarget :: Model -> Int -> Int -> Int -> View Model Action
renderClickTarget m _n r c =
  let gs = mGameState m
      side = turnSide gs
      aiBlocked = mGameMode m == AiMode && mAiSide m == side
      blocked = mAiThinking m || aiBlocked || finished (gsResult gs)
      cur = if blocked then "not-allowed" else "pointer"
  in SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ "transparent"
    , style_ [("cursor", cur), ("touch-action", "manipulation")]
    , SVG.onClick (CellClicked (Coords r c))
    ]

-- ---------------------------------------------------------------------------
-- Status & Controls
-- ---------------------------------------------------------------------------

viewStatus :: Model -> View Model Action
viewStatus m =
  let gs     = mGameState m
      n      = boardSize (gsBoard gs)
      result = gsResult gs
      side   = turnSide gs
      caps   = gsCaptures gs
      isAi   = mGameMode m == AiMode
      (cls, msg)
        | finished result = case winner result of
            Just AttackerSide -> ("card p-3 my-4 w-full text-center font-bold text-destructive", "Attackers win! " <> desc result)
            Just DefenderSide -> ("card p-3 my-4 w-full text-center font-bold", "Defenders win! " <> desc result)
            Nothing           -> ("card p-3 my-4 w-full text-center font-bold", "Draw! " <> desc result)
        | mAiThinking m = ("text-center my-4 py-1 text-muted-foreground animate-pulse font-bold", "AI thinking...")
        | isAi && mAiSide m == side = ("text-center my-4 py-1 font-bold",
            (if side == AttackerSide then "Attacker's turn" else "Defender's turn") <> " (AI)")
        | isAi = ("text-center my-4 py-1 font-bold", "Your turn")
        | side == AttackerSide = ("text-center my-4 py-1 font-bold", "Attacker's turn")
        | otherwise            = ("text-center my-4 py-1 font-bold", "Defender's turn")
  in H.div_
    [ HP.class_ cls
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ text (ms msg)
    , if not (null caps) && not (finished result)
        then H.div_
          [ HP.class_ "mt-1 text-xs opacity-80 font-normal"
          ]
          [ text (ms ("Last move captured " ++ show (length caps) ++ " piece(s)")) ]
        else text ""
    , if finished result
        then case mGameId m of
          Just gid -> viewShareSection m gid
          Nothing  -> text ""
        else text ""
    ]

viewShareSection :: Model -> MisoString -> View Model Action
viewShareSection m gid =
  let shortGid = ms (take 8 (fromMisoString gid :: String))
  in H.div_
    [ HP.class_ "mt-3 pt-3 border-t border-border font-normal"
    ]
    [ H.div_
        [ HP.class_ "text-sm text-muted-foreground text-center mb-2" ]
        [ text ("taflhouse.com/games/" <> shortGid <> "\x2026") ]
    , H.div_
        [ HP.class_ "flex justify-center gap-2" ]
        [ H.button_
            [ HP.class_ "btn btn-outline btn-sm text-foreground"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick CopyGameLink
            ]
            [ text "Copy Link" ]
        , case mSession m of
            Just _ -> H.button_
              [ HP.class_ (if mIsPublic m
                  then "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                  else "btn btn-outline btn-sm text-foreground")
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick ToggleGamePublic
              ]
              [ text (if mIsPublic m then "\x25CF Public" else "\x25CB Private") ]
            Nothing -> text ""
        ]
    ]

viewMoveHistory :: Model -> View Model Action
viewMoveHistory m
  | null (mHistory m) = H.div_ [] []
  | otherwise =
      let allStates = mHistory m ++ [mGameState m]
          n = boardSize (gsBoard (mGameState m))
          currentIdx = length allStates - 1
      in H.div_
        [ HP.class_ "flex flex-col gap-1 items-center w-full"
        , style_ [("max-width", ms (sqSize * n) <> "px")]
        ]
        [ H.div_
            [ HP.class_ "flex justify-between items-center w-full"
            , style_ [("margin-bottom", "0.4em")]
            ]
            [ H.span_
                [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase" ]
                [ text "HISTORY" ]
            , ctrlBtn Undo "Undo"
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ moveBtn m i gs n (i == currentIdx)
            | (i, gs) <- reverse (zip [0..] allStates)
            ]
        ]

moveBtn :: Model -> Int -> GameState -> Int -> Bool -> View Model Action
moveBtn m idx gs n isCurrent =
  let moveSide = opponentSide gs  -- side that made this move
      isHuman = case gsLastAction gs of
        Nothing -> False
        Just _  -> mGameMode m == LocalMode || mAiSide m /= moveSide
      -- Current position: > prefix; human moves: bold; AI moves: dim
      pointer = if isCurrent then "> " else "  "
      moveLabel = case gsLastAction gs of
        Nothing -> "Start"
        Just (MoveAction f t) ->
          let sideChar = case moveSide of
                AttackerSide -> "A"
                DefenderSide -> "D"
          in ms (show idx) <> ". " <> sideChar <> " "
               <> ms (coordStr n f) <> "-" <> ms (coordStr n t)
      label = pointer <> moveLabel
      activeCls = if isCurrent then " text-green-500 dark:text-green-400 border-l-2 border-green-500 dark:border-green-400" else " border-l-2 border-transparent"
      boldCls = if isHuman || isCurrent then " font-bold" else " text-muted-foreground"
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted/50 cursor-pointer bg-transparent border-0 text-foreground" <> activeCls <> boldCls)
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (GotoMove idx)
    ]
    [ text label ]

coordStr :: Int -> Coords -> String
coordStr n (Coords r c) = [toEnum (fromEnum 'a' + c)] ++ show (n - r)


ctrlBtn :: Action -> MisoString -> View Model Action
ctrlBtn action label =
  H.button_
    [ HP.class_ "btn btn-outline btn-sm text-foreground"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]
