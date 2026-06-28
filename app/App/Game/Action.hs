module App.Game.Action (GameAction(..)) where

import Miso.String (MisoString)
import Miso.JSON (Value)
import Supabase.Miso.Realtime (Channel)

import Tafl.Board (Coords, MoveAction)

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
  -- Internal
  | GNoOp
