module App.Game.Model
  ( GameModel(..)
  , GameProps(..)
  , GameRefs(..)
  , VoiceState(..)
  , VideoViewMode(..)
  , initialGameModel
  ) where

import Data.IORef (IORef)
import Miso.String (MisoString)
import Miso.DSL (JSVal)
import Supabase.Miso.Auth (Session)
import Supabase.Miso.Realtime (Channel)

import Tafl.Board (Coords, Side(..), MoveAction)
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (initialState)
import Tafl.Game.State (GameState)

import App.JSON (Profile, ChatMessage)
import App.Model (GameMode(..), TimeControl(..), ViewMode(..), GameInitData(..))

-- | Video overlay view mode.
data VideoViewMode = VideoPiP | VideoTheater
  deriving (Eq, Show)

-- | Voice chat state machine.
data VoiceState
  = VoiceIdle
  | VoiceInviteSent
  | VoiceInviteReceived
  | VoiceConnecting
  | VoiceConnected
  deriving (Eq, Show)

-- | Mutable refs shared between the update function and the outside world.
data GameRefs = GameRefs
  { grChannelRef      :: IORef (Maybe Channel)  -- game realtime channel
  , grChatChannelRef  :: IORef (Maybe Channel)  -- chat realtime channel
  , grClockRef        :: IORef (Maybe Int)       -- clock interval ID
  , grVoiceChannelRef :: IORef (Maybe Channel)  -- voice broadcast channel
  , grPeerConnRef     :: IORef (Maybe JSVal)    -- RTCPeerConnection
  , grMediaStreamRef  :: IORef (Maybe JSVal)    -- local MediaStream
  , grVideoStreamRef  :: IORef (Maybe JSVal)    -- local video MediaStream
  }

-- | Props passed from the root component to the game component.
data GameProps = GameProps
  { gpSession   :: Maybe Session
  , gpProfile   :: Maybe Profile
  , gpGuestName :: Maybe MisoString
  , gpInitData  :: GameInitData
  , gpIsRated   :: !Bool
  } deriving (Eq)

-- | Game component model.
data GameModel = GameModel
  { -- Core game
    gmGameState    :: !GameState
  , gmSelected     :: Maybe Coords
  , gmValidMoves   :: [Coords]
  , gmHistory      :: [GameState]
  , gmBrowseIndex  :: Maybe Int
  , gmMoveList     :: [MoveAction]
  , gmEvalScore    :: !Int
  , gmFullHistory  :: Maybe [GameState]
  , gmFullMoveList :: Maybe [MoveAction]
  , gmAnimateMove  :: Maybe MoveAction
  , gmGameId       :: Maybe MisoString
  , gmVariant      :: !BoardVariant
  , gmGameMode     :: !GameMode
    -- AI
  , gmAiSide       :: !Side
  , gmAiThinking   :: !Bool
  , gmAiDepth      :: !Int
  , gmAiNodeLimit  :: !Int
    -- Multiplayer
  , gmIsRated       :: !Bool
  , gmInviteCode    :: Maybe MisoString
  , gmQrDataUrl     :: Maybe MisoString
  , gmOpponentName  :: Maybe MisoString
  , gmPlayerSide    :: Maybe Side
  , gmAttackerName  :: Maybe MisoString
  , gmDefenderName  :: Maybe MisoString
  , gmDrawOffered   :: !Bool
  , gmSpectatorCount :: !Int
  , gmOpponentOnline  :: !Bool
  , gmOpponentNotice  :: Maybe MisoString
  , gmRematchOffered  :: !Bool
  , gmRematchPending  :: !Bool
  , gmRematchGameId   :: Maybe MisoString
  , gmAttackerId      :: Maybe MisoString
  , gmDefenderId      :: Maybe MisoString
    -- Time control
  , gmTimeControl    :: !TimeControl
  , gmAttackerTimeMs :: !Int
  , gmDefenderTimeMs :: !Int
  , gmLastMoveAt     :: Maybe MisoString
  , gmMoveDeadline   :: Maybe MisoString
  , gmDailyTick      :: !Int
    -- View mode
  , gmViewMode     :: !ViewMode
  , gmIsFullscreen  :: !Bool
  , gmZenHint      :: !Bool
    -- Chat
  , gmChatMessages     :: [ChatMessage]
  , gmChatOpen         :: !Bool
  , gmChatInput        :: !MisoString
  , gmChatUnread       :: !Int
  , gmShowSpectatorChat :: !Bool
    -- Voice
  , gmVoiceState :: !VoiceState
  , gmVoiceMuted :: !Bool
  , gmVoiceError :: Maybe MisoString
    -- Video
  , gmCameraOn      :: !Bool
  , gmRemoteVideoOn :: !Bool
  , gmVideoViewMode :: !VideoViewMode
  } deriving (Eq)

initialGameModel :: GameModel
initialGameModel = GameModel
  { gmGameState    = initialState Tablut
  , gmSelected     = Nothing
  , gmValidMoves   = []
  , gmHistory      = []
  , gmBrowseIndex  = Nothing
  , gmMoveList     = []
  , gmEvalScore    = 0
  , gmFullHistory  = Nothing
  , gmFullMoveList = Nothing
  , gmAnimateMove  = Nothing
  , gmGameId       = Nothing
  , gmVariant      = Tablut
  , gmGameMode     = AiMode
  , gmAiSide       = DefenderSide
  , gmAiThinking   = False
  , gmAiDepth      = 4
  , gmAiNodeLimit  = 10000
  , gmIsRated      = True
  , gmInviteCode   = Nothing
  , gmQrDataUrl    = Nothing
  , gmOpponentName  = Nothing
  , gmPlayerSide    = Nothing
  , gmAttackerName  = Nothing
  , gmDefenderName  = Nothing
  , gmDrawOffered  = False
  , gmSpectatorCount = 0
  , gmOpponentOnline  = True
  , gmOpponentNotice  = Nothing
  , gmRematchOffered  = False
  , gmRematchPending  = False
  , gmRematchGameId   = Nothing
  , gmAttackerId      = Nothing
  , gmDefenderId      = Nothing
  , gmTimeControl  = NoTimeControl
  , gmAttackerTimeMs = 0
  , gmDefenderTimeMs = 0
  , gmLastMoveAt   = Nothing
  , gmMoveDeadline = Nothing
  , gmDailyTick    = 0
  , gmViewMode     = NormalView
  , gmIsFullscreen = False
  , gmZenHint      = False
  , gmChatMessages     = []
  , gmChatOpen         = False
  , gmChatInput        = ""
  , gmChatUnread       = 0
  , gmShowSpectatorChat = False
  , gmVoiceState   = VoiceIdle
  , gmVoiceMuted   = False
  , gmVoiceError   = Nothing
  , gmCameraOn      = False
  , gmRemoteVideoOn = False
  , gmVideoViewMode = VideoPiP
  }
