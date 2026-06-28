module App.Model
  ( -- * Enums
    GameMode(..)
  , TimeControl(..)
  , Screen(..)
  , ViewMode(..)
  , DeferredMpAction(..)
    -- * Model
  , Model(..)
  , eqSession
  , initModel
  ) where

import Miso.String (MisoString)
import Supabase.Miso.Auth (Session(..))
import Supabase.Miso.Realtime (Channel)

import Tafl.Board (Coords, Side(..), MoveAction)
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (GameState, initialState)
import Tafl.Game.State (GameResult)

import App.JSON (Profile, GameRecord)

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
-- Model
-- ---------------------------------------------------------------------------

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
  , mBrowseIndex    :: Maybe Int
  , mSession        :: Maybe Session
  , mAuthEmail      :: !MisoString
  , mAuthPassword   :: !MisoString
  , mAuthError      :: Maybe MisoString
  , mAuthMessage    :: Maybe MisoString
  , mAuthLoading    :: !Bool
  , mPastGames      :: [GameRecord]
  , mGamesLoading   :: !Bool
  , mConfigExpanded :: !Bool
  , mConfigModeChosen :: !Bool
  , mShowQuoteRef  :: !Bool
  , mQuoteRefGen   :: !Int
  , mToast         :: Maybe MisoString
  , mShowDepthInfo :: !Bool
  , mShowNodesInfo :: !Bool
  , mLocalGames    :: [GameRecord]
  , mMoveList      :: [MoveAction]
  , mReplayGame    :: Maybe GameRecord
  , mReplayStates  :: [GameState]
  , mReplayIndex   :: !Int
  , mGameId        :: Maybe MisoString
  , mReplayNotFound  :: !Bool
  -- Multiplayer
  , mGuestName        :: Maybe MisoString
  , mRealtimeChannel :: Maybe Channel
  , mProfile         :: Maybe Profile
  , mNeedsUsername    :: !Bool
  , mUsernameInput   :: !MisoString
  , mInviteCode      :: Maybe MisoString
  , mQrDataUrl       :: Maybe MisoString
  , mJoinCodeInput   :: !MisoString
  , mOpponentName    :: Maybe MisoString
  , mPlayerSide      :: Maybe Side
  , mDrawOffered     :: !Bool
  , mProfileDropdown :: !Bool
  , mSidePreference  :: !MisoString
  , mEditUsername    :: !MisoString
  , mEditDisplayName :: !MisoString
  , mDeferredMpAction :: Maybe DeferredMpAction
  , mEvalScore        :: !Int
  , mFullHistory      :: Maybe [GameState]   -- ^ Preserved full state list for browsing
  , mFullMoveList     :: Maybe [MoveAction]  -- ^ Preserved full move list for browsing
  , mViewMode         :: !ViewMode
  , mIsFullscreen     :: !Bool
  , mZenHint          :: !Bool
  -- Time control
  , mTimeControl       :: !TimeControl
  , mAttackerTimeMs    :: !Int
  , mDefenderTimeMs    :: !Int
  , mLastMoveAt        :: Maybe MisoString
  , mMoveDeadline      :: Maybe MisoString
  , mClockTimerId      :: Maybe Int          -- JS setInterval ID
  , mDailyTick         :: !Int               -- bumped every 30s to force re-render
  }

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
        && mBrowseIndex a == mBrowseIndex b
        && eqSession (mSession a) (mSession b)
        && mAuthEmail a == mAuthEmail b
        && mAuthPassword a == mAuthPassword b
        && mAuthError a == mAuthError b
        && mAuthMessage a == mAuthMessage b
        && mAuthLoading a == mAuthLoading b
        && mPastGames a == mPastGames b
        && mGamesLoading a == mGamesLoading b
        && mConfigExpanded a == mConfigExpanded b
        && mConfigModeChosen a == mConfigModeChosen b
        && mShowQuoteRef a == mShowQuoteRef b
        && mQuoteRefGen a == mQuoteRefGen b
        && mToast a == mToast b
        && mShowDepthInfo a == mShowDepthInfo b
        && mShowNodesInfo a == mShowNodesInfo b
        && mLocalGames a == mLocalGames b
        && mMoveList a == mMoveList b
        && mReplayGame a == mReplayGame b
        && mReplayStates a == mReplayStates b
        && mReplayIndex a == mReplayIndex b
        && mGameId a == mGameId b
        && mReplayNotFound a == mReplayNotFound b
        && mGuestName a == mGuestName b
        && mProfile a == mProfile b
        && mNeedsUsername a == mNeedsUsername b
        && mUsernameInput a == mUsernameInput b
        && mInviteCode a == mInviteCode b
        && mQrDataUrl a == mQrDataUrl b
        && mJoinCodeInput a == mJoinCodeInput b
        && mOpponentName a == mOpponentName b
        && mPlayerSide a == mPlayerSide b
        && mDrawOffered a == mDrawOffered b
        && mProfileDropdown a == mProfileDropdown b
        && mSidePreference a == mSidePreference b
        && mEditUsername a == mEditUsername b
        && mEditDisplayName a == mEditDisplayName b
        && mDeferredMpAction a == mDeferredMpAction b
        && mEvalScore a == mEvalScore b
        && mFullHistory a == mFullHistory b
        && mFullMoveList a == mFullMoveList b
        && mViewMode a == mViewMode b
        && mIsFullscreen a == mIsFullscreen b
        && mZenHint a == mZenHint b
        && mTimeControl a == mTimeControl b
        && mAttackerTimeMs a == mAttackerTimeMs b
        && mDefenderTimeMs a == mDefenderTimeMs b
        && mLastMoveAt a == mLastMoveAt b
        && mMoveDeadline a == mMoveDeadline b
        && mDailyTick a == mDailyTick b

eqSession :: Maybe Session -> Maybe Session -> Bool
eqSession Nothing Nothing = True
eqSession (Just x) (Just y) = sessionAccessToken x == sessionAccessToken y
eqSession _ _ = False

-- ---------------------------------------------------------------------------
-- Initial model
-- ---------------------------------------------------------------------------

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
  , mBrowseIndex    = Nothing
  , mSession        = Nothing
  , mAuthEmail      = ""
  , mAuthPassword   = ""
  , mAuthError      = Nothing
  , mAuthMessage    = Nothing
  , mAuthLoading    = False
  , mPastGames      = []
  , mGamesLoading   = False
  , mConfigExpanded = False
  , mConfigModeChosen = False
  , mShowQuoteRef  = False
  , mQuoteRefGen   = 0
  , mToast         = Nothing
  , mShowDepthInfo = False
  , mShowNodesInfo = False
  , mLocalGames    = []
  , mMoveList      = []
  , mReplayGame    = Nothing
  , mReplayStates  = []
  , mReplayIndex   = 0
  , mGameId        = Nothing
  , mReplayNotFound  = False
  , mGuestName        = Nothing
  , mRealtimeChannel = Nothing
  , mProfile         = Nothing
  , mNeedsUsername    = False
  , mUsernameInput   = ""
  , mInviteCode      = Nothing
  , mQrDataUrl       = Nothing
  , mJoinCodeInput   = ""
  , mOpponentName    = Nothing
  , mPlayerSide      = Nothing
  , mDrawOffered     = False
  , mProfileDropdown = False
  , mSidePreference  = "defender"
  , mEditUsername    = ""
  , mEditDisplayName = ""
  , mDeferredMpAction = Nothing
  , mEvalScore        = 0
  , mFullHistory      = Nothing
  , mFullMoveList     = Nothing
  , mViewMode         = NormalView
  , mIsFullscreen     = False
  , mZenHint          = False
  , mTimeControl      = NoTimeControl
  , mAttackerTimeMs   = 0
  , mDefenderTimeMs   = 0
  , mLastMoveAt       = Nothing
  , mMoveDeadline     = Nothing
  , mClockTimerId     = Nothing
  , mDailyTick        = 0
  }
