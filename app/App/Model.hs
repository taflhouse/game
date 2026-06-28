module App.Model
  ( -- * Enums
    GameMode(..)
  , TimeControl(..)
  , Screen(..)
  , ViewMode(..)
  , DeferredMpAction(..)
    -- * Game init data
  , GameInitData(..)
    -- * Model
  , Model(..)
  , initModel
  ) where

import Miso.String (MisoString)
import Supabase.Miso.Auth (Session)

import Tafl.Board (Side(..), MoveAction)
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (GameState, initialState)

import App.JSON (Profile, GameRow, GameRecord)

-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

data GameMode = PracticeMode | AiMode | MultiplayerMode
  deriving (Eq, Show)

data TimeControl
  = NoTimeControl
  | BlitzControl !Int    -- total milliseconds per player
  | DailyControl !Int    -- seconds per move
  deriving (Eq, Show)

data Screen = HomeScreen | SignInScreen | SignUpScreen | ConfigScreen | ConfigureScreen | JoinScreen | GameScreen | ReplayScreen | ProfileScreen | ProfileEditScreen | LoadingScreen
  deriving (Eq, Show)

data DeferredMpAction = DeferCreate | DeferJoin
  deriving (Eq, Show)

data ViewMode = NormalView | ZenView
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Game init data (shared between root and game component)
-- ---------------------------------------------------------------------------

data GameInitData
  = NewLocalGame !MisoString !BoardVariant !GameMode !Side !Int !Int
    -- ^ uuid variant mode aiSide aiDepth aiNodeLimit
  | NewMultiplayerGame !BoardVariant !TimeControl !MisoString !MisoString !MisoString !MisoString
    -- ^ variant timeControl sidePreference invCode uuid qrDataUrl
  | JoinGame !GameRow
    -- ^ joining via invite code (player not yet in the row)
  | ResumeGame !GameRow
    -- ^ resuming an existing game via /play/<uuid>
  deriving (Eq)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data Model = Model
  { -- Navigation
    mScreen           :: !Screen
  , mGameMode         :: !GameMode
  , mVariant          :: !BoardVariant
  , mGameInitData     :: Maybe GameInitData
  , mReplayGameId     :: Maybe MisoString
    -- AI config (set in configure, passed to game via props)
  , mAiSide           :: !Side
  , mAiDepth          :: !Int
  , mAiNodeLimit      :: !Int
    -- Auth
  , mSession          :: Maybe Session
  , mAuthEmail        :: !MisoString
  , mAuthPassword     :: !MisoString
  , mAuthError        :: Maybe MisoString
  , mAuthMessage      :: Maybe MisoString
  , mAuthLoading      :: !Bool
    -- Profile
  , mProfile          :: Maybe Profile
  , mNeedsUsername    :: !Bool
  , mUsernameInput    :: !MisoString
  , mEditUsername     :: !MisoString
  , mEditDisplayName  :: !MisoString
  , mProfileDropdown  :: !Bool
    -- Home
  , mPastGames        :: [GameRecord]
  , mGamesLoading     :: !Bool
  , mLocalGames       :: [GameRecord]
  , mShowQuoteRef     :: !Bool
  , mQuoteRefGen      :: !Int
    -- Replay (stays in root until Phase 2)
  , mReplayGame       :: Maybe GameRecord
  , mReplayStates     :: [GameState]
  , mReplayIndex      :: !Int
  , mReplayNotFound   :: !Bool
  , mEvalScore        :: !Int
    -- Multiplayer config
  , mSidePreference   :: !MisoString
  , mTimeControl      :: !TimeControl
  , mJoinCodeInput    :: !MisoString
  , mGuestName        :: Maybe MisoString
  , mDeferredMpAction :: Maybe DeferredMpAction
    -- View mode (used by replay; game component manages its own)
  , mViewMode         :: !ViewMode
  , mIsFullscreen     :: !Bool
  , mZenHint          :: !Bool
    -- UI chrome
  , mConfigExpanded   :: !Bool
  , mConfigModeChosen :: !Bool
  , mToast            :: Maybe MisoString
  , mShowDepthInfo    :: !Bool
  , mShowNodesInfo    :: !Bool
  } deriving (Eq)

-- ---------------------------------------------------------------------------
-- Initial model
-- ---------------------------------------------------------------------------

initModel :: Model
initModel = Model
  { mScreen           = HomeScreen
  , mGameMode         = AiMode
  , mVariant          = Tablut
  , mGameInitData     = Nothing
  , mReplayGameId     = Nothing
  , mAiSide           = DefenderSide
  , mAiDepth          = 4
  , mAiNodeLimit      = 10000
  , mSession          = Nothing
  , mAuthEmail        = ""
  , mAuthPassword     = ""
  , mAuthError        = Nothing
  , mAuthMessage      = Nothing
  , mAuthLoading      = False
  , mProfile          = Nothing
  , mNeedsUsername    = False
  , mUsernameInput    = ""
  , mEditUsername     = ""
  , mEditDisplayName  = ""
  , mProfileDropdown  = False
  , mPastGames        = []
  , mGamesLoading     = False
  , mLocalGames       = []
  , mShowQuoteRef     = False
  , mQuoteRefGen      = 0
  , mReplayGame       = Nothing
  , mReplayStates     = []
  , mReplayIndex      = 0
  , mReplayNotFound   = False
  , mEvalScore        = 0
  , mSidePreference   = "defender"
  , mTimeControl      = NoTimeControl
  , mJoinCodeInput    = ""
  , mGuestName        = Nothing
  , mDeferredMpAction = Nothing
  , mViewMode         = NormalView
  , mIsFullscreen     = False
  , mZenHint          = False
  , mConfigExpanded   = False
  , mConfigModeChosen = False
  , mToast            = Nothing
  , mShowDepthInfo    = False
  , mShowNodesInfo    = False
  }
