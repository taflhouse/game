module Tafl.Capture
  ( checkCaptures
  ) where

import Tafl.Types

-- | Four orthogonal directions as (row delta, col delta).
dir4 :: [(Int, Int)]
dir4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]

-- | Whether a square can help the given side perform a capture.
-- A square helps capture if it contains a friendly piece (including
-- an armed king for defenders) or is an empty base (corner / empty throne).
canHelpCapture :: GameState -> Coords -> Side -> Bool
canHelpCapture gs coords side
  | not (insideBounds board coords) = False
  | isEmptyBase gs coords = True
  | otherwise = case (side, pieceAt board coords) of
      (AttackerSide, Attacker) -> True
      (DefenderSide, Defender) -> True
      (DefenderSide, King)     -> kingIsArmed (gsRules gs)
      _                        -> False
  where
    board = gsBoard gs

-- | Whether a piece at the given coordinates can be captured by the given side.
-- Kings are handled separately (need surrounding), so they're excluded here.
canBeCaptured :: GameState -> Coords -> Side -> Bool
canBeCaptured gs coords side =
  let board = gsBoard gs
      p     = pieceAt board coords
  in case (side, p) of
    (AttackerSide, Defender) -> True   -- attackers capture defenders (not king)
    (DefenderSide, Attacker) -> True   -- defenders capture attackers
    _                        -> False

-- | Whether a square is an empty base (corner or empty throne).
isEmptyBase :: GameState -> Coords -> Bool
isEmptyBase gs coords =
  isBase gs coords && isEmpty (gsBoard gs) coords

-- | After a piece moves to @landing@, check all captures.
-- Standard sandwich captures in 4 directions, plus shield walls if enabled.
checkCaptures :: GameState -> Coords -> [Coords]
checkCaptures gs landing =
  let side    = turnSide gs
      board   = gsBoard gs
      -- Standard sandwich captures
      sandwiches = concatMap (checkSandwich gs landing side) dir4
      -- King capture check
      kingCaps = checkKingCapture gs landing side
      -- Shield wall captures
      walls = if shieldWalls (gsRules gs)
              then checkShieldWalls gs landing
              else []
  in sandwiches ++ kingCaps ++ walls

-- | Check one direction from the landing square for a sandwich capture.
checkSandwich :: GameState -> Coords -> Side -> (Int, Int) -> [Coords]
checkSandwich gs landing side (dr, dc) =
  let board  = gsBoard gs
      mid    = Coords (row landing + dr) (col landing + dc)
      flank  = Coords (row landing + 2 * dr) (col landing + 2 * dc)
  in if insideBounds board mid
        && insideBounds board flank
        && canHelpCapture gs flank side
        && canBeCaptured gs mid side
     then [mid]
     else []

-- | Check if the king is captured. Looks at all 4 neighbors of the landing
-- square; if one is the king, check if the king is surrounded on enough sides.
checkKingCapture :: GameState -> Coords -> Side -> [Coords]
checkKingCapture gs landing side
  | side /= AttackerSide = []  -- only attackers can capture the king
  | otherwise = concatMap checkNeighbor dir4
  where
    board  = gsBoard gs
    needed = attackerCountToCapture (gsRules gs)

    checkNeighbor (dr, dc) =
      let kingPos = Coords (row landing + dr) (col landing + dc)
      in if insideBounds board kingPos && isKing board kingPos
         then let surrounded = length $ filter (\c -> canHelpCapture gs c AttackerSide) $
                    map (\(dr',dc') -> Coords (row kingPos + dr') (col kingPos + dc')) dir4
              in if surrounded >= needed then [kingPos] else []
         else []

-- | Shield wall captures. When a piece moves to an edge, consecutive enemy
-- pieces along that edge can be captured if each has a friendly piece behind
-- it (one row/column inward) and the run is terminated by a friendly piece
-- or capture helper on the edge. The king is immune to shield wall capture.
checkShieldWalls :: GameState -> Coords -> [Coords]
checkShieldWalls gs landing =
  let board = gsBoard gs
      n     = boardSize board
      side  = turnSide gs
      opp   = case side of AttackerSide -> DefenderSide; DefenderSide -> AttackerSide
      lr    = row landing
      lc    = col landing
      -- Horizontal edge: scan left and right along columns
      hCaps = if lr == 0 || lr == n - 1
              then let rowBehind = if lr == n - 1 then n - 2 else 1
                   in scanEdge gs side opp lr lc (-1) rowBehind True
                   ++ scanEdge gs side opp lr lc 1    rowBehind True
              else []
      -- Vertical edge: scan up and down along rows
      vCaps = if lc == 0 || lc == n - 1
              then let colBehind = if lc == n - 1 then n - 2 else 1
                   in scanEdge gs side opp lc lr (-1) colBehind False
                   ++ scanEdge gs side opp lc lr 1    colBehind False
              else []
  in hCaps ++ vCaps

-- | Scan along an edge from the landing position in one direction,
-- collecting capturable enemy pieces. The scan continues while:
--   1. The next position on the edge is an opponent piece (not king)
--   2. The position behind it (one row/col inward) has a capture helper
-- If the scan terminates at a friendly capture helper on the edge,
-- all collected pieces are captured. Otherwise nothing is captured.
--
-- @isHorizontal@: True = scanning columns along a row edge,
--                 False = scanning rows along a column edge
scanEdge :: GameState -> Side -> Side -> Int -> Int -> Int -> Int -> Bool -> [Coords]
scanEdge gs side opp edgeIdx startPos delta behindIdx isHorizontal =
  let board = gsBoard gs
      mkCoords pos = if isHorizontal
                     then Coords edgeIdx pos
                     else Coords pos edgeIdx
      mkBehind pos = if isHorizontal
                     then Coords behindIdx pos
                     else Coords pos behindIdx

      -- Walk along the edge collecting opponent pieces
      collect pos acc
        | not (insideBounds board (mkCoords pos)) = (acc, pos)
        | sideOfPiece (pieceAt board (mkCoords pos)) /= Just opp = (acc, pos)
        | not (insideBounds board (mkBehind pos)) = (acc, pos)
        | not (canHelpCapture gs (mkBehind pos) side) = (acc, pos)
        | isKing board (mkCoords pos) = (acc, pos)  -- king is immune
        | otherwise = collect (pos + delta) (mkCoords pos : acc)

      (captured, endPos) = collect (startPos + delta) []

      -- Check if the run is terminated by a friendly capture helper
      endCoords = mkCoords endPos
  in if insideBounds board endCoords && canHelpCapture gs endCoords side
     then captured
     else []
