{-# LANGUAGE LambdaCase #-}
module App.Route
  ( Route(..)
  , parseRoute
  , variantSlugMs
  , variantName
  , replayMoves
  , friendlyAuthError
  , lookupVariant
    -- * URI helpers
  , homeURI
  , signInURI
  , signUpURI
  , configURI
  , configureURI
  , playURI
  , gamePermalinkURI
  , profileURI
  , profileEditURI
  , joinURI
  , joinBareURI
  ) where

import Data.List (isPrefixOf)
import Miso (URI(..), emptyURI)
import Miso.String (MisoString, ms, fromMisoString)

import Tafl.Board (MoveAction)
import Tafl.Rules (BoardVariant(..), variantSlug)
import Tafl.Game (act, GameState)

data Route = HomeRoute | SignInRoute | SignUpRoute | ConfigRoute | ConfigureRoute | ProfileRoute | ProfileEditRoute
           | PlayRoute MisoString      -- /play/<uuid> active game
           | GameRoute MisoString      -- /games/<uuid> replay/permalink
           | JoinRoute (Maybe MisoString) -- /join or /join/<invite_code>

variantSlugMs :: BoardVariant -> MisoString
variantSlugMs v = ms (variantSlug v)

variantName :: BoardVariant -> MisoString
variantName = \case
  Brandubh      -> "Brandubh 7x7"
  Tablut        -> "Tablut 9x9"
  Classic       -> "Copenhagen 11x11"
  Line          -> "Line 11x11"
  Tawlbwrdd     -> "Tawlbwrdd 11x11"
  Lewis         -> "Lewis 11x11"
  Parlett       -> "Parlett 13x13"
  DamienWalker  -> "Damien Walker 15x15"
  AleaEvangelii -> "Alea Evangelii 19x19"

parseRoute :: URI -> Route
parseRoute uri = case uriPath uri of
  "sign-in"  -> SignInRoute
  "sign-up"  -> SignUpRoute
  "new-game" -> ConfigRoute
  "new-game/configure" -> ConfigureRoute
  "profile/edit" -> ProfileEditRoute
  "profile"  -> ProfileRoute
  "join"     -> JoinRoute Nothing
  path
    | Just uuid <- msStripPrefix "play/" path
    , isUUID uuid -> PlayRoute uuid
    | Just uuid <- msStripPrefix "games/" path
    , isUUID uuid -> GameRoute uuid
    | Just code <- msStripPrefix "join/" path
    , not (null (fromMisoString code :: String)) -> JoinRoute (Just code)
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
lookupVariant slug = lookup slug [ (variantSlugMs v, v) | v <- [minBound .. maxBound] ]

-- | Replay a list of moves from an initial state, returning
--   (intermediateStates, finalState) so both mHistory and mGameState
--   can be populated in one pass.
replayMoves :: GameState -> [MoveAction] -> ([GameState], GameState)
replayMoves gs0 moves =
  let states = scanl act gs0 moves        -- gs0 : gs1 : ... : gsN
  in  (init states, last states)           -- history = all but last, final = last
  -- scanl always produces at least one element (gs0), so these are safe

friendlyAuthError :: MisoString -> MisoString
friendlyAuthError code = case (fromMisoString code :: String) of
  "email_not_confirmed" -> "Please check your email and confirm your account before signing in."
  "invalid_credentials" -> "Invalid email or password."
  "user_already_exists" -> "An account with this email already exists."
  _ -> "Something went wrong. Please try again."

homeURI :: URI
homeURI = emptyURI

signInURI :: URI
signInURI = emptyURI { uriPath = "sign-in" }

signUpURI :: URI
signUpURI = emptyURI { uriPath = "sign-up" }

configURI :: URI
configURI = emptyURI { uriPath = "new-game" }

configureURI :: URI
configureURI = emptyURI { uriPath = "new-game/configure" }

playURI :: MisoString -> URI
playURI uuid = emptyURI { uriPath = "play/" <> uuid }

gamePermalinkURI :: MisoString -> URI
gamePermalinkURI uuid = emptyURI { uriPath = "games/" <> uuid }

profileURI :: URI
profileURI = emptyURI { uriPath = "profile" }

profileEditURI :: URI
profileEditURI = emptyURI { uriPath = "profile/edit" }

joinURI :: MisoString -> URI
joinURI code = emptyURI { uriPath = "join/" <> code }

joinBareURI :: URI
joinBareURI = emptyURI { uriPath = "join" }
