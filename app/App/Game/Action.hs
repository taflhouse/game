module App.Game.Action (GameAction(..)) where

import Miso.String (MisoString)
import Miso.JSON (Value)
import Miso.DSL (JSVal)
import Supabase.Miso.Realtime (Channel)

import Tafl.Board (Coords, MoveAction)

import App.Game.Model (VideoViewMode)

data GameAction
  = GameMount
  | GameUnmount
  -- Game play
  | GCellClicked Coords
  | GAiMoveComplete MoveAction
  | GGotoMove Int
  | GUndo
  -- Multiplayer
  | GRealtimeChange Value
  | GPresenceSync Value
  | GRealtimeSubscribed Channel
  | GRealtimeError MisoString
  | GMoveUpdated Value
  | GMoveUpdateError MisoString
  | GWriteMpMoveWithClock MisoString (Maybe MisoString)
  | GCompleteJoinWithClock MisoString MisoString MisoString (Maybe MisoString)
    -- ^ (uid, displayName, nowStr, mDeadlineStr)
  | GResign
  | GOfferDraw
  | GAcceptDraw
  | GDeclineDraw
  | GCopyGameLink
  | GCopyInviteCode MisoString
  | GSetQrDataUrl MisoString
  -- Time control
  | GClockTick Int Int
  | GClockTimeout MisoString
  | GClockStarted Int
  | GStopClock
  | GDailyTick
  -- View mode
  | GToggleZenMode
  | GDismissZenHint
  | GToggleFullscreen
  -- Persistence
  | GGameSaved Value
  | GGameSaveError MisoString
  | GGameCreated Value
  | GGameCreateError MisoString
  -- Chat
  | GToggleChat
  | GSetChatInput MisoString
  | GSendChat
  | GChatInserted Value
  | GChatInsertError MisoString
  | GChatReceived Value
  | GChatSubscribed Channel
  | GChatError MisoString
  | GToggleSpectatorChat
  | GChatHistoryLoaded Value
  | GChatHistoryError MisoString
  -- Voice chat
  | GVoiceInvite
  | GVoiceAccept
  | GVoiceDecline
  | GVoiceEnd
  | GVoiceToggleMute
  | GVoiceBroadcastReceived Value
  | GVoiceBroadcastSubscribed Channel
  | GVoiceBroadcastError MisoString
  | GVoiceGotMedia JSVal
  | GVoiceMediaError MisoString
  | GVoiceOfferCreated MisoString
  | GVoiceOfferError MisoString
  | GVoiceAnswerCreated MisoString
  | GVoiceAnswerError MisoString
  | GVoiceRemoteAnswerSet
  | GVoiceRemoteAnswerError MisoString
  | GVoiceIceCandidate MisoString
  | GVoiceIceCandidateAdded
  | GVoiceIceCandidateError MisoString
  | GVoiceRemoteTrack
  -- Video
  | GVideoToggleCamera
  | GVideoGotMedia JSVal
  | GVideoMediaError MisoString
  | GVideoRemoteTrackOn
  | GVideoRemoteTrackOff
  | GVideoSetViewMode VideoViewMode
  -- Rating
  | GRatingUpdated Value
  | GRatingUpdateError MisoString
  -- Presence
  | GDismissNotice
  -- Rematch
  | GRequestRematch
  | GAcceptRematch
  | GDeclineRematch
  | GRematchInserted Value
  | GRematchInsertError MisoString
  -- Internal
  | GNoOp
