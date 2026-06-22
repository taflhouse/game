module Tafl.AI (AiConfig(..), defaultAiConfig, bestMove, evaluate) where

import Data.List (sortBy, foldl')
import Data.Ord (comparing, Down(..))

import Tafl.Types
import Tafl.Game (act)
import Tafl.Move (getPossibleActions)

-- | AI configuration.
data AiConfig = AiConfig
  { acMaxDepth :: !Int   -- ^ Iterative deepening limit (1..8)
  , acMaxNodes :: !Int   -- ^ Hard node cutoff (0 = unlimited)
  } deriving (Eq, Show)

-- | Default AI configuration: depth 4, 10k node limit.
defaultAiConfig :: AiConfig
defaultAiConfig = AiConfig { acMaxDepth = 4, acMaxNodes = 10000 }

-- | Find the best move for the current side using minimax with alpha-beta
-- pruning and iterative deepening. Returns 'Nothing' if the game is over
-- or no legal moves exist.
bestMove :: AiConfig -> GameState -> Maybe MoveAction
bestMove config gs
  | finished (gsResult gs) = Nothing
  | otherwise = case orderMoves gs' (getPossibleActions gs') of
      []             -> Nothing
      moves@(m0:_)   -> Just (iterDeepen moves m0 1 m0)
  where
    gs' = prepareForSearch gs
    isMax = turnSide gs' == AttackerSide

    iterDeepen :: [MoveAction] -> MoveAction -> Int -> MoveAction -> MoveAction
    iterDeepen moves m0 depth prevBest
      | depth > acMaxDepth config = prevBest
      | otherwise =
          let (mv, nodes) = searchRoot moves m0 depth
              exceeded = acMaxNodes config > 0 && nodes >= acMaxNodes config
          in if exceeded && depth > 1
             then prevBest
             else iterDeepen moves m0 (depth + 1) mv

    searchRoot :: [MoveAction] -> MoveAction -> Int -> (MoveAction, Int)
    searchRoot moves m0 depth =
      let initScore = if isMax then -200000 else 200000
      in rootLoop moves m0 initScore (-200000) 200000 0
      where
        rootLoop :: [MoveAction] -> MoveAction -> Int -> Int -> Int -> Int -> (MoveAction, Int)
        rootLoop [] best _ _ _ nodes = (best, nodes)
        rootLoop (mv:rest) best bestSc a b nodes
          | acMaxNodes config > 0 && nodes >= acMaxNodes config = (best, nodes)
          | otherwise =
              let gs'' = act gs' mv
                  (sc, nodes') = alphaBeta config gs'' (depth - 1) a b nodes
              in if isMax
                 then let best' = if sc > bestSc then mv else best
                          bestSc' = max bestSc sc
                          a' = max a sc
                      in rootLoop rest best' bestSc' a' b nodes'
                 else let best' = if sc < bestSc then mv else best
                          bestSc' = min bestSc sc
                          b' = min b sc
                      in rootLoop rest best' bestSc' a b' nodes'

-- | Alpha-beta search. Score is always from AttackerSide perspective:
-- positive = good for attackers, negative = good for defenders.
-- Returns (score, nodeCount).
alphaBeta :: AiConfig -> GameState -> Int -> Int -> Int -> Int -> (Int, Int)
alphaBeta config gs depth alpha beta nodes
  | depth == 0 || finished (gsResult gs) = (evaluate gs, nodes + 1)
  | turnSide gs == AttackerSide = goMax moves alpha nodes
  | otherwise = goMin moves beta nodes
  where
    moves = orderMoves gs (getPossibleActions gs)

    goMax :: [MoveAction] -> Int -> Int -> (Int, Int)
    goMax [] a n = (a, n)
    goMax _ a n | acMaxNodes config > 0 && n >= acMaxNodes config = (a, n)
    goMax (mv:rest) a n =
      let gs' = act gs mv
          (sc, n') = alphaBeta config gs' (depth - 1) a beta n
          a' = max a sc
      in if a' >= beta then (a', n')
         else goMax rest a' n'

    goMin :: [MoveAction] -> Int -> Int -> (Int, Int)
    goMin [] b n = (b, n)
    goMin _ b n | acMaxNodes config > 0 && n >= acMaxNodes config = (b, n)
    goMin (mv:rest) b n =
      let gs' = act gs mv
          (sc, n') = alphaBeta config gs' (depth - 1) alpha b n
          b' = min b sc
      in if b' <= alpha then (b', n')
         else goMin rest b' n'

-- | Strip board history and action tracking to avoid expensive D4 symmetry
-- computation in every 'act' call during the search tree.
prepareForSearch :: GameState -> GameState
prepareForSearch gs = gs
  { gsRules = (gsRules gs)
      { saveBoardHistory = False, saveActions = False, skipExpensiveChecks = True }
  , gsBoardHistory = mempty
  , gsActions = []
  }

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | Static evaluation from AttackerSide perspective.
--
-- Components:
--   Terminal:      +/-100000 for win/loss, 0 for draw
--   Material:      Attacker*100, Defender*150, King*500
--   King distance: -80..0  (negative = king close to corner = good for defenders)
--   King exposure: +40 per adjacent attacker
--   Mobility:      x2 * (attacker moves - defender moves)
--   Board control: x1 per piece (attackers near center, defenders near edges)
evaluate :: GameState -> Int
evaluate gs
  | finished result = case winner result of
      Just AttackerSide ->  100000
      Just DefenderSide -> -100000
      Nothing           ->  0
  | otherwise = material + kingDist + kingExpo + boardCtrl
  where
    result = gsResult gs
    board  = gsBoard gs
    n      = boardSize board
    center = n `div` 2

    (aCnt, dCnt, mKing) = countPieces board n

    -- Asymmetric material: defenders are fewer, more valuable
    material = aCnt * 100 - dCnt * 150
             - case mKing of { Nothing -> 0; Just _ -> 500 }

    -- King distance to nearest corner (negative = close = good for defenders)
    kingDist = case mKing of
      Nothing -> 0
      Just kc ->
        let w = cornerBaseWidth (gsRules gs)
            corners = [ Coords r c
                      | r <- concatMap (\ww -> [ww, n - 1 - ww]) [0 .. w - 1]
                      , c <- concatMap (\ww -> [ww, n - 1 - ww]) [0 .. w - 1]
                      ]
            minD = minimum (map (manhattan kc) corners)
            maxD = 2 * (n - 1)
        in negate (80 * (maxD - minD) `div` max 1 maxD)

    -- King exposure: adjacent attackers threaten capture
    kingExpo = case mKing of
      Nothing -> 0
      Just kc -> 40 * length
        [ ()
        | (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
        , let sq = Coords (row kc + dr) (col kc + dc)
        , insideBounds board sq
        , isAttacker board sq
        ]

    -- Board control: attackers rewarded near center, defenders near edges
    boardCtrl = sum [ ctrl r c | r <- [0 .. n - 1], c <- [0 .. n - 1] ]

    ctrl r c = case pieceAt board (Coords r c) of
      Attacker -> center - (abs (r - center) + abs (c - center))
      Defender -> negate (center - minimum [r, c, n - 1 - r, n - 1 - c])
      King     -> negate (center - minimum [r, c, n - 1 - r, n - 1 - c])
      Empty    -> 0

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

manhattan :: Coords -> Coords -> Int
manhattan (Coords r1 c1) (Coords r2 c2) = abs (r1 - r2) + abs (c1 - c2)

-- | Count attackers, defenders, and locate the king.
countPieces :: Board -> Int -> (Int, Int, Maybe Coords)
countPieces board n = foldl' go (0, 0, Nothing)
  [ (r, c) | r <- [0 .. n - 1], c <- [0 .. n - 1] ]
  where
    go (!a, !d, k) (r, c) = case pieceAt board (Coords r c) of
      Attacker -> (a + 1, d, k)
      Defender -> (a, d + 1, k)
      King     -> (a, d, Just (Coords r c))
      Empty    -> (a, d, k)

-- | Order moves for better alpha-beta pruning using cheap heuristics.
-- No 'act' calls — just static board inspection.
orderMoves :: GameState -> [MoveAction] -> [MoveAction]
orderMoves gs = sortBy (comparing (Down . priority))
  where
    board = gsBoard gs
    side  = turnSide gs
    priority mv =
      let piece = pieceAt board (from mv)
          dest  = to mv
      in if piece == King && isCorner gs dest then 1000
         else if piece == King then 10
         else if hasAdjacentEnemy board dest side then 5
         else (0 :: Int)

-- | Check if a destination square is adjacent to an enemy piece (capture proxy).
hasAdjacentEnemy :: Board -> Coords -> Side -> Bool
hasAdjacentEnemy board (Coords r c) side =
  let n = boardSize board
      enemy = case side of
        AttackerSide -> isDefenderOrKing
        DefenderSide -> isAttacker
      check dr dc =
        let r' = r + dr; c' = c + dc
        in r' >= 0 && r' < n && c' >= 0 && c' < n && enemy board (Coords r' c')
  in check (-1) 0 || check 1 0 || check 0 (-1) || check 0 1
