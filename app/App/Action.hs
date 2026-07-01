module App.Action (Action(..)) where

import Miso (URI)
import Miso.String (MisoString)
import Miso.JSON (Value)
import Supabase.Miso.Auth (AuthResponse, Session)

import Tafl.Board (Side)
import Tafl.Rules (BoardVariant)

import App.Model (GameMode, TimeControl)
import App.JSON (GameRecord)

data Action
  = NoOp
  -- Navigation
  | StartGame
  | StartGameWithId MisoString
  | GotoHome
  | GotoSignIn
  | GotoSignUp
  | GotoConfig
  | GotoJoin
  | ToggleConfigExpand
  | HandleURI URI
  -- Game config
  | SetGameMode GameMode
  | SetVariant BoardVariant
  | SetAiSide Side
  | SetAiDepth Int
  | SetAiNodeLimit Int
  -- Auth
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
  | CheckSession
  | ValidateSession Session
  | SessionRestored (Maybe Session)
  -- Home
  | GamesLoaded Value
  | GamesLoadError MisoString
  | ToggleTheme
  | ToggleQuoteRef
  | DismissQuoteRef
  | DismissQuoteRefTimed Int
  | LocalGamesLoaded [GameRecord]
  | DoMigrateGames MisoString [GameRecord]
  -- Toast
  | ShowToast MisoString
  | DismissToast
  -- Config UI
  | ToggleDepthInfo
  | ToggleNodesInfo
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
  -- Multiplayer setup
  | CreateMultiplayerGame
  | InitMultiplayerGame MisoString MisoString MisoString  -- invCode uuid qrDataUrl
  | JoinMultiplayerGame
  | GameFoundToJoin Value
  | GameJoinError MisoString
  | ResumeGameLoaded Value
  | ResumeGameLoadError MisoString
  | SetJoinCodeInput MisoString
  | SetJoinNameInput MisoString
  | SetSidePreference MisoString
  | SetTimeControl TimeControl
  -- Replay
  | GotoReplay MisoString
  -- View mode (replay only; game component handles its own)
  | DocumentDblClick
  | ToggleZenMode
  | DismissZenHint
  | ToggleFullscreen
  | Undo
  -- Game component mailbox
  | GameMailbox Value
