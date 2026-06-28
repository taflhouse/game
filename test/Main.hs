module Main where

import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Vector as V

import Tafl.Board
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (act, initialState, GameState(..), GameResult(..))
import Tafl.Game.Move (getPossibleActions, isActionPossible)
import Tafl.Game.Symmetry (canonicalBoardKey, rotate90, mirrorBoard, symmetryVariants)

-- ---------------------------------------------------------------------------
-- Minimal model simulation (mirrors app/Main.hs update logic)
-- ---------------------------------------------------------------------------

data GameMode = SPractice | SAi | SMultiplayer deriving (Eq, Show)

data SimModel = SimModel
  { simGameState   :: GameState
  , simHistory     :: [GameState]
  , simMoveList    :: [MoveAction]
  , simBrowseIndex :: Maybe Int
  , simGameMode    :: GameMode
  , simFullHistory :: Maybe [GameState]
  , simFullMoveList :: Maybe [MoveAction]
  } deriving (Show)

initModel :: BoardVariant -> SimModel
initModel v = initModelWithMode v SPractice

initModelWithMode :: BoardVariant -> GameMode -> SimModel
initModelWithMode v mode = SimModel
  { simGameState   = initialState v
  , simHistory     = []
  , simMoveList    = []
  , simBrowseIndex = Nothing
  , simGameMode    = mode
  , simFullHistory = Nothing
  , simFullMoveList = Nothing
  }

-- | The game state currently being displayed (browsed or live).
displayedState :: SimModel -> GameState
displayedState m = case simBrowseIndex m of
  Nothing -> simGameState m
  Just i  -> let allSt = simHistory m ++ [simGameState m]
             in if i >= 0 && i < length allSt
                then allSt !! i
                else simGameState m

-- | Apply a move (mirrors CellClicked handler).
applyMove :: SimModel -> MoveAction -> SimModel
applyMove m move
  | simGameMode m == SMultiplayer && simBrowseIndex m /= Nothing = m
  | otherwise =
      let activeGs = displayedState m
          gs' = act activeGs move
          (newHist, newMoves) = case simBrowseIndex m of
            Just i  -> (take i (simHistory m ++ [simGameState m]),
                        take i (simMoveList m))
            Nothing -> (simHistory m, simMoveList m)
      in m { simGameState   = gs'
           , simHistory     = newHist ++ [activeGs]
           , simMoveList    = newMoves ++ [move]
           , simBrowseIndex = Nothing
           , simFullHistory = Nothing
           , simFullMoveList = Nothing
           }

-- | Undo one move (mirrors Undo handler).
undo :: SimModel -> SimModel
undo m = case simHistory m of
  [] -> m
  _  ->
    let prev       = last (simHistory m)
        newHistory  = init (simHistory m)
        fh = case simFullHistory m of
          Just fs -> Just fs
          Nothing -> Just (simHistory m ++ [simGameState m])
        fm = case simFullMoveList m of
          Just ms' -> Just ms'
          Nothing  -> Just (simMoveList m)
    in m { simGameState   = prev
         , simHistory     = newHistory
         , simMoveList    = take (length newHistory) (simMoveList m)
         , simFullHistory = fh
         , simFullMoveList = fm
         }

-- | Go to a specific move index (mirrors GotoMove handler).
gotoMove :: Int -> SimModel -> SimModel
gotoMove i m =
  let allSt = simHistory m ++ [simGameState m]
      lastIdx = length allSt - 1
      idx = if i >= lastIdx then Nothing else Just (max 0 i)
  in m { simBrowseIndex = idx }

allStates :: SimModel -> [GameState]
allStates m = simHistory m ++ [simGameState m]

isGameOver :: SimModel -> Bool
isGameOver = finished . gsResult . simGameState

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

firstLegalMove :: SimModel -> Maybe MoveAction
firstLegalMove m = case getPossibleActions (displayedState m) of
  []    -> Nothing
  (a:_) -> Just a

applyNMoves :: Int -> SimModel -> SimModel
applyNMoves 0 m = m
applyNMoves n m = case firstLegalMove m of
  Nothing   -> m
  Just move -> applyNMoves (n - 1) (applyMove m move)

playToEnd :: Int -> SimModel -> SimModel
playToEnd 0 m = m
playToEnd n m
  | finished (gsResult (simGameState m)) = m
  | otherwise = case firstLegalMove m of
      Nothing   -> m
      Just move -> playToEnd (n - 1) (applyMove m move)

historyLen :: SimModel -> Int
historyLen = length . simHistory

moveListLen :: SimModel -> Int
moveListLen = length . simMoveList

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  let m0 = initModel Brandubh

  describe "History invariant (length mHistory == length mMoveList)" $ do
    let checkInvariant m = historyLen m `shouldBe` moveListLen m

    it "initial state" $
      checkInvariant m0

    it "after 1 move" $
      checkInvariant (applyNMoves 1 m0)

    it "after 5 moves" $
      checkInvariant (applyNMoves 5 m0)

    it "1 move then undo" $
      checkInvariant (undo (applyNMoves 1 m0))

    it "5 moves then undo" $
      checkInvariant (undo (applyNMoves 5 m0))

    it "5 moves then 3 undos" $
      checkInvariant (undo . undo . undo $ applyNMoves 5 m0)

    it "undo all moves" $
      checkInvariant (undo . undo . undo . undo . undo $ applyNMoves 5 m0)

    it "undo on empty history" $
      checkInvariant (undo m0)

    it "goto move 0 from 5 moves" $
      checkInvariant (gotoMove 0 (applyNMoves 5 m0))

    it "goto move 2 from 5 moves" $
      checkInvariant (gotoMove 2 (applyNMoves 5 m0))

    it "goto current position (no-op)" $
      checkInvariant (gotoMove 5 (applyNMoves 5 m0))

    it "goto out of bounds (no-op)" $
      checkInvariant (gotoMove 99 (applyNMoves 5 m0))

    it "goto move 3 then undo" $
      checkInvariant (undo . gotoMove 3 $ applyNMoves 5 m0)

    it "goto move 2 then make 2 new moves" $
      checkInvariant (applyNMoves 2 . gotoMove 2 $ applyNMoves 5 m0)

    it "undo then make new move" $ do
      let m = applyNMoves 1 . undo $ applyNMoves 3 m0
      checkInvariant m

    it "all variants start clean" $ do
      let variants = [minBound .. maxBound] :: [BoardVariant]
      mapM_ (\v -> checkInvariant (initModel v)) variants

  describe "Browse index" $ do
    it "does not mutate history" $ do
      let m5 = applyNMoves 5 m0
          m2 = gotoMove 2 m5
      historyLen m2 `shouldBe` historyLen m5
      moveListLen m2 `shouldBe` moveListLen m5

    it "sets browseIndex" $ do
      let m2 = gotoMove 2 (applyNMoves 5 m0)
      simBrowseIndex m2 `shouldBe` Just 2

    it "goto last move clears browseIndex" $ do
      let m5' = gotoMove 5 . gotoMove 2 $ applyNMoves 5 m0
      simBrowseIndex m5' `shouldBe` Nothing

    it "displayedState shows browsed state" $ do
      let m5 = applyNMoves 5 m0
          m2 = gotoMove 2 m5
      displayedState m2 `shouldBe` allStates m5 !! 2

    it "displayedState shows current when not browsing" $ do
      let m5 = applyNMoves 5 m0
      displayedState m5 `shouldBe` simGameState m5

    it "all states preserved while browsing" $ do
      let m2 = gotoMove 2 (applyNMoves 5 m0)
      length (allStates m2) `shouldBe` 6

  describe "Single player: browse and fork" $ do
    it "can make move while browsing (fork)" $ do
      let m3 = applyNMoves 1 . gotoMove 2 $ applyNMoves 5 m0
      length (allStates m3) `shouldBe` 4
      historyLen m3 `shouldBe` moveListLen m3

    it "fork clears browseIndex" $ do
      let m3 = applyNMoves 1 . gotoMove 2 $ applyNMoves 5 m0
      simBrowseIndex m3 `shouldBe` Nothing

    it "fork truncates future moves" $ do
      let m3 = applyNMoves 1 . gotoMove 2 $ applyNMoves 5 m0
      moveListLen m3 `shouldBe` 3

    it "fork from move 0 replaces all history" $ do
      let m1 = applyNMoves 1 . gotoMove 0 $ applyNMoves 5 m0
      moveListLen m1 `shouldBe` 1
      historyLen m1 `shouldBe` 1

    it "multiple forks maintain invariant" $ do
      let m = applyNMoves 1 . gotoMove 1 . applyNMoves 1 . gotoMove 2 $ applyNMoves 5 m0
      historyLen m `shouldBe` moveListLen m

  describe "Multiplayer: browse but no fork" $ do
    let m0mp = initModelWithMode Brandubh SMultiplayer

    it "can browse history" $ do
      let m2 = gotoMove 2 (applyNMoves 5 m0mp)
      simBrowseIndex m2 `shouldNotBe` Nothing

    it "browse does not mutate history" $ do
      let m5 = applyNMoves 5 m0mp
          m2 = gotoMove 2 m5
      historyLen m2 `shouldBe` historyLen m5
      moveListLen m2 `shouldBe` moveListLen m5

    it "move blocked while browsing" $ do
      let m5 = applyNMoves 5 m0mp
          m2 = gotoMove 2 m5
          m2' = applyNMoves 1 m2
      simBrowseIndex m2' `shouldBe` simBrowseIndex m2
      moveListLen m2' `shouldBe` moveListLen m2

    it "can return to latest after browsing" $ do
      let m5' = gotoMove 5 . gotoMove 2 $ applyNMoves 5 m0mp
      simBrowseIndex m5' `shouldBe` Nothing

    it "history unchanged after browse round-trip" $ do
      let m5 = applyNMoves 5 m0mp
          m5' = gotoMove 5 . gotoMove 2 $ m5
      simGameState m5' `shouldBe` simGameState m5
      historyLen m5' `shouldBe` historyLen m5
      moveListLen m5' `shouldBe` moveListLen m5

  describe "Game over" $ do
    it "persists when browsing back" $ do
      let mDone = playToEnd 500 m0
      if not (isGameOver mDone) then pure ()
      else isGameOver (gotoMove 2 mDone) `shouldBe` True

    it "reverts after undo" $ do
      let mDone = playToEnd 500 m0
      if not (isGameOver mDone) then pure ()
      else isGameOver (undo mDone) `shouldBe` False

    it "persists after multiple gotos" $ do
      let mDone = playToEnd 500 m0
      if not (isGameOver mDone) then pure ()
      else isGameOver (gotoMove 1 . gotoMove 0 $ mDone) `shouldBe` True

  -- -----------------------------------------------------------------------
  -- Property-based tests: Board symmetry
  -- -----------------------------------------------------------------------

  describe "Board symmetry properties" $ do
    it "rotate90 four times is identity" $ hedgehog $ do
      board <- forAll genBoard
      rotate90 (rotate90 (rotate90 (rotate90 board))) === board

    it "mirror twice is identity" $ hedgehog $ do
      board <- forAll genBoard
      mirrorBoard (mirrorBoard board) === board

    it "canonicalBoardKey is invariant under rotation" $ hedgehog $ do
      board <- forAll genBoard
      let key = canonicalBoardKey board
      canonicalBoardKey (rotate90 board) === key

    it "canonicalBoardKey is invariant under mirror" $ hedgehog $ do
      board <- forAll genBoard
      let key = canonicalBoardKey board
      canonicalBoardKey (mirrorBoard board) === key

    it "canonicalBoardKey is invariant under all 8 symmetry variants" $ hedgehog $ do
      board <- forAll genBoard
      let key = canonicalBoardKey board
          keys = map canonicalBoardKey (symmetryVariants board)
      keys === replicate 8 key

    it "symmetryVariants produces exactly 8 boards" $ hedgehog $ do
      board <- forAll genBoard
      length (symmetryVariants board) === 8

    it "rotation preserves board dimensions" $ hedgehog $ do
      board <- forAll genBoard
      let rotated = rotate90 board
      V.length rotated === V.length board
      V.length (V.head rotated) === V.length (V.head board)

    it "mirror preserves board dimensions" $ hedgehog $ do
      board <- forAll genBoard
      let mirrored = mirrorBoard board
      V.length mirrored === V.length board
      V.length (V.head mirrored) === V.length (V.head board)

  -- -----------------------------------------------------------------------
  -- Property-based tests: Movement/action consistency
  -- -----------------------------------------------------------------------

  describe "Movement/action consistency" $ do
    it "every action in getPossibleActions passes isActionPossible" $ hedgehog $ do
      gs <- forAll genGameState
      let actions = getPossibleActions gs
      annotateShow (length actions)
      assert $ all (isActionPossible gs) actions

    it "every action is orthogonal (same row or same column)" $ hedgehog $ do
      gs <- forAll genGameState
      let actions = getPossibleActions gs
      assert $ all (\a -> row (from a) == row (to a)
                       || col (from a) == col (to a)) actions

    it "no action has from == to" $ hedgehog $ do
      gs <- forAll genGameState
      let actions = getPossibleActions gs
      assert $ all (\a -> from a /= to a) actions

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genPiece :: Gen Piece
genPiece = Gen.element [Empty, Attacker, Defender, King]

-- | Generate a random square board with odd side length (matching real Tafl boards).
genBoard :: Gen Board
genBoard = do
  half <- Gen.int (Range.linear 3 9)
  let n = 2 * half + 1   -- odd sizes: 7, 9, 11, ..., 19
  V.replicateM n (V.replicateM n genPiece)

-- | Generate a game state by playing random legal moves from an initial position.
genGameState :: Gen GameState
genGameState = do
  variant <- Gen.element [minBound .. maxBound]
  let gs0 = initialState variant
  numMoves <- Gen.int (Range.linear 0 30)
  playRandomMoves numMoves gs0

-- | Play up to n random legal moves, stopping early if the game ends.
playRandomMoves :: Int -> GameState -> Gen GameState
playRandomMoves 0 gs = pure gs
playRandomMoves n gs
  | finished (gsResult gs) = pure gs
  | otherwise = case getPossibleActions gs of
      [] -> pure gs
      actions -> do
        move <- Gen.element actions
        playRandomMoves (n - 1) (act gs move)
