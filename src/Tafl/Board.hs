{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Tafl.Board
  ( -- * Piece
    Piece(..)
  , Side(..)
    -- * Coordinates
  , Coords(..)
  , MoveAction(..)
    -- * Board
  , Board
  , boardSize
  , pieceAt
  , insideBounds
    -- * Piece predicates
  , isEmpty
  , isKing
  , isAttacker
  , isDefender
  , isDefenderOrKing
  , sideOfPiece
  , canControl
    -- * Board position predicates
  , isCenter
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Piece types on the board.
data Piece = Empty | Attacker | Defender | King
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The two sides in a tafl game.
data Side = AttackerSide | DefenderSide
  deriving (Eq, Ord, Show)

-- | Board coordinates (0-indexed, row and column).
data Coords = Coords { row :: !Int, col :: !Int }
  deriving (Eq, Ord, Show)

-- | A move from one position to another.
data MoveAction = MoveAction { from :: !Coords, to :: !Coords }
  deriving (Eq, Show)

-- | The game board — a square grid of pieces.
type Board = Vector (Vector Piece)

-- ---------------------------------------------------------------------------
-- Board helpers
-- ---------------------------------------------------------------------------

-- | Side length of a square board.
boardSize :: Board -> Int
boardSize = V.length

-- | Piece at a given coordinate.
pieceAt :: Board -> Coords -> Piece
pieceAt board (Coords r c) = (board V.! r) V.! c

-- | Whether coordinates are within board bounds.
insideBounds :: Board -> Coords -> Bool
insideBounds board (Coords r c) =
  let n = V.length board
  in r >= 0 && r < n && c >= 0 && c < n

-- ---------------------------------------------------------------------------
-- Piece predicates
-- ---------------------------------------------------------------------------

isEmpty :: Board -> Coords -> Bool
isEmpty board coords = pieceAt board coords == Empty

isKing :: Board -> Coords -> Bool
isKing board coords = pieceAt board coords == King

isAttacker :: Board -> Coords -> Bool
isAttacker board coords = pieceAt board coords == Attacker

isDefender :: Board -> Coords -> Bool
isDefender board coords = pieceAt board coords == Defender

isDefenderOrKing :: Board -> Coords -> Bool
isDefenderOrKing board coords =
  let p = pieceAt board coords
  in p == Defender || p == King

-- | Which side does a piece belong to?
sideOfPiece :: Piece -> Maybe Side
sideOfPiece Attacker = Just AttackerSide
sideOfPiece Defender = Just DefenderSide
sideOfPiece King     = Just DefenderSide
sideOfPiece Empty    = Nothing

-- | Whether a side can move a given piece.
canControl :: Side -> Piece -> Bool
canControl AttackerSide Attacker = True
canControl DefenderSide Defender = True
canControl DefenderSide King     = True
canControl _            _        = False

-- ---------------------------------------------------------------------------
-- Position predicates
-- ---------------------------------------------------------------------------

-- | Whether coordinates are the center (throne) square.
isCenter :: Board -> Coords -> Bool
isCenter board (Coords r c) =
  let n = V.length board
      center = n `div` 2
  in r == center && c == center

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

instance ToJSON Coords where
  toJSON (Coords r c) = toJSON [r, c]

instance FromJSON Coords where
  parseJSON v = do
    [r, c] <- parseJSON v
    pure (Coords r c)

instance ToJSON MoveAction where
  toJSON (MoveAction f t) = toJSON [toJSON f, toJSON t]

instance FromJSON MoveAction where
  parseJSON v = do
    [f, t] <- parseJSON v
    pure (MoveAction f t)

instance ToJSON Side where
  toJSON AttackerSide = toJSON ("attacker" :: Text)
  toJSON DefenderSide = toJSON ("defender" :: Text)

instance FromJSON Side where
  parseJSON = withText "Side" $ \case
    "attacker" -> pure AttackerSide
    "defender" -> pure DefenderSide
    _          -> fail "expected \"attacker\" or \"defender\""
