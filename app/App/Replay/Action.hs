module App.Replay.Action (ReplayAction(..)) where

import Miso.JSON (Value)
import Miso.String (MisoString)

data ReplayAction
  = ReplayMount
  | ReplayUnmount
  | RReplayLoaded Value
  | RReplayLoadError MisoString
  | RGotoMove Int
  | RToggleZen
  | RToggleFullscreen
  | RNoOp
