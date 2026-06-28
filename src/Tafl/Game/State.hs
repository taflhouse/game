{-# LANGUAGE OverloadedStrings #-}
module Tafl.Game.State
  ( -- * Game state
    GameState(..)
  , GameResult(..)
    -- * Turn logic
  , turnSide
  , opponentSide
    -- * Position predicates (need GameState for rules)
  , isCorner
  , isEdge
  , isBase
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withObject, (.:), object, (.=))
import Data.Map.Strict (Map)
import Data.Text (Text)

import Tafl.Board
import Tafl.Rules (RuleSet(..))

-- | Result of a game (or in-progress indicator).
data GameResult = GameResult
  { finished :: !Bool
  , winner   :: Maybe Side
  , desc     :: !Text
  } deriving (Eq, Show)

-- | Complete game state.
data GameState = GameState
  { gsBoard        :: !Board
  , gsActions      :: [MoveAction]
  , gsBoardHistory :: Map Text Int
  , gsTurn         :: !Int
  , gsResult       :: !GameResult
  , gsCaptures     :: [Coords]
  , gsLastAction   :: Maybe MoveAction
  , gsRules        :: !RuleSet
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Turn logic
-- ---------------------------------------------------------------------------

-- | Which side's turn is it?
turnSide :: GameState -> Side
turnSide gs =
  let evenTurn = gsTurn gs `mod` 2 == 0
      attackerStarts = startingSide (gsRules gs) == AttackerSide
  in if attackerStarts == evenTurn
     then AttackerSide
     else DefenderSide

-- | The opponent of the current turn's side.
opponentSide :: GameState -> Side
opponentSide gs = case turnSide gs of
  AttackerSide -> DefenderSide
  DefenderSide -> AttackerSide

-- ---------------------------------------------------------------------------
-- Position predicates (need GameState for cornerBaseWidth)
-- ---------------------------------------------------------------------------

-- | Whether coordinates are a corner square (width determined by rules).
isCorner :: GameState -> Coords -> Bool
isCorner gs (Coords r c) =
  let n = boardSize (gsBoard gs)
      w = cornerBaseWidth (gsRules gs)
      rowMatch = any (\ww -> r == ww || r == n - 1 - ww) [0 .. w - 1]
      colMatch = any (\ww -> c == ww || c == n - 1 - ww) [0 .. w - 1]
  in rowMatch && colMatch

-- | Whether coordinates are on the board edge but not a corner.
isEdge :: GameState -> Coords -> Bool
isEdge gs (Coords r c) =
  let n = boardSize (gsBoard gs)
      onEdge = r == 0 || r == n - 1 || c == 0 || c == n - 1
  in onEdge && not (isCorner gs (Coords r c))

-- | Whether coordinates are a "base" square (center or corner).
isBase :: GameState -> Coords -> Bool
isBase gs coords = isCenter (gsBoard gs) coords || isCorner gs coords

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

instance ToJSON GameResult where
  toJSON (GameResult fin w d) = object
    [ "finished" .= fin
    , "winner"   .= w
    , "desc"     .= d
    ]

instance FromJSON GameResult where
  parseJSON = withObject "GameResult" $ \v ->
    GameResult <$> v .: "finished" <*> v .: "winner" <*> v .: "desc"
