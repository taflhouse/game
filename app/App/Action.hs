module App.Action (Action(..)) where

import Miso (URI)
import Miso.String (MisoString)
import Miso.JSON (Value)
import Supabase.Miso.Auth (AuthResponse, Session)
import Supabase.Miso.Realtime (Channel)

import Tafl.Board (Coords, MoveAction, Side)
import Tafl.Rules (BoardVariant)

import App.Model (GameMode, TimeControl)
import App.JSON (GameRecord)

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
  | GotoJoin
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
  | AnonAuthSuccess AuthResponse
  | AnonAuthError MisoString
  | SignOutSuccess Value
  | SessionRestored (Maybe Session)
  | GameSaved Value
  | GameSaveError MisoString
  | GamesLoaded Value
  | GamesLoadError MisoString
  | ToggleTheme
  | ToggleQuoteRef
  | DismissQuoteRef
  | DismissQuoteRefTimed Int
  | ShowToast MisoString
  | DismissToast
  | SetQrDataUrl MisoString
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
  | GameUpdated Value
  | GameUpdateError MisoString
  | CopyGameLink
  -- Profile
  | SetUsernameInput MisoString
  | SubmitUsername
  | ProfileCreated Value
  | ProfileCreateError MisoString
  | ProfileLoaded Value
  | ProfileLoadError MisoString
  | ToggleProfileDropdown
  | GotoProfile
  | GotoProfileEdit
  | SetEditUsername MisoString
  | SetEditDisplayName MisoString
  | SubmitProfileEdit
  | ProfileUpdated Value
  | ProfileUpdateError MisoString
  -- Multiplayer
  | CreateMultiplayerGame
  | InitMultiplayerGame MisoString MisoString MisoString  -- invCode uuid qrDataUrl
  | JoinMultiplayerGame
  | GameFoundToJoin Value
  | GameJoinError MisoString
  | GameJoinedOk Value
  | GameJoinUpdateError MisoString
  | RealtimeChange Value
  | RealtimeSubscribed Channel
  | RealtimeError MisoString
  | MoveUpdated Value
  | MoveUpdateError MisoString
  | ResumeGameLoaded Value
  | ResumeGameLoadError MisoString
  | SetJoinCodeInput MisoString
  | Resign
  | OfferDraw
  | AcceptDraw
  | DeclineDraw
  | SetSidePreference MisoString
  | CopyInviteCode MisoString
  | ToggleZenMode
  | DismissZenHint
  | DocumentDblClick
  | ToggleFullscreen
  -- Time control
  | SetTimeControl TimeControl
  | ClockTick Int Int           -- (attackerMs, defenderMs) from JS timer
  | ClockTimeout MisoString     -- "attacker" | "defender" from JS
  | ClockStarted Int            -- JS interval ID
  | StopClock
  | DailyTick                   -- periodic re-render for daily countdown
  | WriteMpMoveWithClock MisoString (Maybe MisoString)
    -- ^ (nowStr, mDeadlineStr) Continue multiplayer move DB write with IO-computed timestamps
  | CompleteJoinWithClock MisoString MisoString MisoString (Maybe MisoString)
    -- ^ (uid, displayName, nowStr, mDeadlineStr) Continue game join DB write
