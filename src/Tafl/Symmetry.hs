{-# LANGUAGE OverloadedStrings #-}
module Tafl.Symmetry
  ( canonicalBoardKey
  , addBoardToHistory
  , checkRepetition
  , initialBoardHistory
  , rotate90
  , mirrorBoard
  , symmetryVariants
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Tafl.Types

-- | Convert a piece to a character for board serialization.
pieceToChar :: Piece -> Char
pieceToChar Empty    = '0'
pieceToChar Attacker = '1'
pieceToChar Defender = '2'
pieceToChar King     = '3'

-- | Serialize a board to a text key.
boardToKey :: Board -> Text
boardToKey board = T.intercalate "|" $ V.toList $ V.map rowToText board
  where
    rowToText r = T.pack $ V.toList $ V.map pieceToChar r

-- | Rotate a board 90 degrees clockwise.
rotate90 :: Board -> Board
rotate90 board =
  let n = V.length board
  in V.generate n (\r ->
       V.generate n (\c ->
         pieceAt board (Coords (n - 1 - c) r)))

-- | Mirror a board horizontally (reverse rows).
mirrorBoard :: Board -> Board
mirrorBoard = V.reverse

-- | Generate all 8 D4 symmetry variants of a board.
-- 4 rotations of the original + 4 rotations of the mirror.
symmetryVariants :: Board -> [Board]
symmetryVariants board =
  let r1 = rotate90 board
      r2 = rotate90 r1
      r3 = rotate90 r2
      m  = mirrorBoard board
      m1 = rotate90 m
      m2 = rotate90 m1
      m3 = rotate90 m2
  in [board, r1, r2, r3, m, m1, m2, m3]

-- | Canonical board key: the lexicographically smallest key among
-- all 8 D4 symmetry variants. Two board positions that differ only
-- by rotation or reflection produce the same canonical key.
canonicalBoardKey :: Board -> Text
canonicalBoardKey board = minimum $ map boardToKey $ symmetryVariants board

-- | Add the current board position to the history, incrementing
-- the count for its canonical key.
addBoardToHistory :: GameState -> GameState
addBoardToHistory gs =
  let key = canonicalBoardKey (gsBoard gs)
      hist = gsBoardHistory gs
      count = Map.findWithDefault 0 key hist + 1
  in gs { gsBoardHistory = Map.insert key count hist }

-- | Check if the current board position has been repeated enough
-- times to trigger a draw.
checkRepetition :: GameState -> Bool
checkRepetition gs =
  let key = canonicalBoardKey (gsBoard gs)
      limit = repetitionTurnLimit (gsRules gs)
  in Map.findWithDefault 0 key (gsBoardHistory gs) >= limit

-- | Create the initial board history with the starting position
-- counted once.
initialBoardHistory :: Board -> Map Text Int
initialBoardHistory board = Map.singleton (canonicalBoardKey board) 1
