{-# LANGUAGE OverloadedStrings #-}
module Tafl.Game
  ( act
  , isGameOver
  , initialState
    -- * Re-exports from Game.State
  , module Tafl.Game.State
  ) where

import qualified Data.Vector as V
import Tafl.Board
import Tafl.Rules (RuleSet(..), BoardVariant, variantDefaultRules)
import Tafl.Game.State
import Tafl.Game.Board (variantBoard)
import Tafl.Game.Move (getPossibleActions, canMakeAMove)
import Tafl.Game.Capture (checkCaptures)
import Tafl.Game.Fort (kingEscapedThroughFort)
import Tafl.Game.Surround (didAttackersSurroundDefenders)
import Tafl.Game.Symmetry (addBoardToHistory, checkRepetition, initialBoardHistory)

-- | Create the initial game state for a given board variant.
initialState :: BoardVariant -> GameState
initialState variant =
  let board = variantBoard variant
      rules = variantDefaultRules variant
  in GameState
    { gsBoard        = board
    , gsActions      = []
    , gsBoardHistory = if saveBoardHistory rules
                       then initialBoardHistory board
                       else mempty
    , gsTurn         = 0
    , gsResult       = GameResult False Nothing ""
    , gsCaptures     = []
    , gsLastAction   = Nothing
    , gsRules        = rules
    }

-- | Apply a move to the game state: move the piece, check captures,
-- advance the turn, and check for game over.
act :: GameState -> MoveAction -> GameState
act gs move =
  let board   = gsBoard gs
      piece   = pieceAt board (from move)
      -- Move the piece
      board1  = setPiece (setPiece board (from move) Empty) (to move) piece
      gs1     = gs { gsBoard = board1 }
      -- Check captures from the landing square
      caps    = checkCaptures gs1 (to move)
      -- Remove captured pieces
      board2  = foldl (\b c -> setPiece b c Empty) board1 caps
      -- Advance turn
      gs2     = gs1
        { gsBoard      = board2
        , gsTurn       = gsTurn gs + 1
        , gsCaptures   = caps
        , gsLastAction = Just move
        , gsActions    = if saveActions (gsRules gs)
                         then gsActions gs ++ [move]
                         else []
        }
      -- Track board history for repetition detection
      gs3     = if saveBoardHistory (gsRules gs)
                then addBoardToHistory gs2
                else gs2
      -- Check game over
      result  = isGameOver gs3
  in gs3 { gsResult = result }

-- | Set a piece at a coordinate on the board.
setPiece :: Board -> Coords -> Piece -> Board
setPiece board (Coords r c) piece =
  let oldRow = board V.! r
      newRow = oldRow V.// [(c, piece)]
  in board V.// [(r, newRow)]

-- | Check if the game is over.
isGameOver :: GameState -> GameResult
isGameOver gs
  -- Draw on threefold repetition
  | saveBoardHistory (gsRules gs) && checkRepetition gs =
      GameResult True Nothing "Draw on repetition"
  -- King reached a corner -> defender wins
  | kingAtCorner gs = GameResult True (Just DefenderSide) "King escaped!"
  -- King escaped through exit fort -> defender wins
  | exitForts (gsRules gs) && kingEscapedThroughFort gs =
      GameResult True (Just DefenderSide) "King escaped through exit fort!"
  -- Attackers surrounded all defenders -> attacker wins
  | not (skipExpensiveChecks (gsRules gs)) && didAttackersSurroundDefenders (gsBoard gs) =
      GameResult True (Just AttackerSide) "Defenders surrounded!"
  -- King captured (no king on board) -> attacker wins
  | not (kingExists gs) = GameResult True (Just AttackerSide) "King captured!"
  -- Current side has no legal moves -> they lose
  | not (canMakeAMove gs (turnSide gs)) =
      GameResult True (Just (opponentSide gs)) "No legal moves!"
  -- Game continues
  | otherwise = GameResult False Nothing ""

-- | Check if the king is on any corner square.
kingAtCorner :: GameState -> Bool
kingAtCorner gs =
  let board = gsBoard gs
      n     = boardSize board
      w     = cornerBaseWidth (gsRules gs)
      corners = [ Coords r c
                | r <- concatMap (\ww -> [ww, n - 1 - ww]) [0 .. w - 1]
                , c <- concatMap (\ww -> [ww, n - 1 - ww]) [0 .. w - 1]
                ]
  in any (\coord -> insideBounds board coord && isKing board coord) corners

-- | Check if any king piece exists on the board.
kingExists :: GameState -> Bool
kingExists gs =
  let board = gsBoard gs
      n     = boardSize board
  in any (\r -> any (\c -> pieceAt board (Coords r c) == King) [0..n-1]) [0..n-1]
