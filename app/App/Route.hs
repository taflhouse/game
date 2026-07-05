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
  , loungeURI
  , yourGamesURI
  , playerURI
  , learnURI
  , learnLessonURI
  ) where

import Data.List (isPrefixOf)
import Miso (URI(..), emptyURI)
import Miso.String (MisoString, ms, fromMisoString)

import Tafl.Board (MoveAction)
import Tafl.Rules (BoardVariant(..), variantSlug)
import Tafl.Game (act, GameState)

import App.Tutorial.Lessons (TutorialLesson(..), moduleSlug, lookupLesson)

data Route = HomeRoute | SignInRoute | SignUpRoute | ConfigRoute | ConfigureRoute (Maybe MisoString) | ProfileRoute | ProfileEditRoute
           | PlayRoute MisoString      -- /play/<uuid> active game
           | GameRoute MisoString      -- /games/<uuid> replay/permalink
           | JoinRoute (Maybe MisoString) -- /join or /join/<invite_code>
           | LoungeRoute               -- /lounge (redirects to home)
           | YourGamesRoute            -- /your-games past games list
           | PlayerRoute MisoString    -- /player/<username>
           | LearnRoute                -- /learn lesson select
           | LearnLessonRoute MisoString -- /learn/<lesson-id>

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
  "lounge"   -> HomeRoute  -- lounge is now the home screen
  "your-games" -> YourGamesRoute
  "sign-in"  -> SignInRoute
  "sign-up"  -> SignUpRoute
  "new-game" -> ConfigRoute
  "new-game/practice"    -> ConfigureRoute (Just "practice")
  "new-game/ai"          -> ConfigureRoute (Just "ai")
  "new-game/multiplayer" -> ConfigureRoute (Just "multiplayer")
  "new-game/configure"   -> ConfigureRoute Nothing
  "profile/edit" -> ProfileEditRoute
  "profile"  -> ProfileRoute
  "join"     -> JoinRoute Nothing
  "learn"    -> LearnRoute
  path
    | Just uuid <- msStripPrefix "play/" path
    , isUUID uuid -> PlayRoute uuid
    | Just uuid <- msStripPrefix "games/" path
    , isUUID uuid -> GameRoute uuid
    | Just code <- msStripPrefix "join/" path
    , not (null (fromMisoString code :: String)) -> JoinRoute (Just code)
    | Just uname <- msStripPrefix "player/" path
    , not (null (fromMisoString uname :: String)) -> PlayerRoute uname
    | Just rest <- msStripPrefix "learn/" path
    , not (null (fromMisoString rest :: String))
    -> case break' '/' (fromMisoString rest :: String) of
        (_, lid) | not (null lid) -> LearnLessonRoute (ms lid)
        (lid, _)                  -> LearnLessonRoute (ms lid)
    | otherwise   -> HomeRoute

msStripPrefix :: String -> MisoString -> Maybe MisoString
msStripPrefix pfx s =
  let str = fromMisoString s :: String
  in if pfx `isPrefixOf` str
     then Just (ms (drop (length pfx) str))
     else Nothing

-- | Split a string on the first occurrence of a character.
-- Returns (before, after) where 'after' does not include the delimiter.
-- If the character is not found, returns (input, "").
break' :: Char -> String -> (String, String)
break' c s = case break (== c) s of
  (a, [])    -> (a, [])
  (a, _:b)   -> (a, b)

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

configureURI :: MisoString -> URI
configureURI modeSlug = emptyURI { uriPath = "new-game/" <> modeSlug }

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

loungeURI :: URI
loungeURI = emptyURI { uriPath = "lounge" }

yourGamesURI :: URI
yourGamesURI = emptyURI { uriPath = "your-games" }

playerURI :: MisoString -> URI
playerURI uname = emptyURI { uriPath = "player/" <> uname }

learnURI :: URI
learnURI = emptyURI { uriPath = "learn" }

learnLessonURI :: MisoString -> URI
learnLessonURI lid = case lookupLesson lid of
  Just lesson -> emptyURI { uriPath = "learn/" <> moduleSlug (tlModule lesson) <> "/" <> lid }
  Nothing     -> emptyURI { uriPath = "learn/" <> lid }
