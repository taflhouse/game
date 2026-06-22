module Main where

import Tafl.Types
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (act, initialState)
import Tafl.Move (getPossibleActions)

-- ---------------------------------------------------------------------------
-- Minimal model simulation (mirrors app/Main.hs update logic)
-- ---------------------------------------------------------------------------

-- | The subset of Model state relevant to the history invariant.
data SimModel = SimModel
  { simGameState :: GameState
  , simHistory   :: [GameState]
  , simMoveList  :: [MoveAction]
  } deriving (Show)

initModel :: BoardVariant -> SimModel
initModel v = SimModel
  { simGameState = initialState v
  , simHistory   = []
  , simMoveList  = []
  }

-- | Apply a move (mirrors CellClicked handler).
applyMove :: SimModel -> MoveAction -> SimModel
applyMove m move =
  let gs  = simGameState m
      gs' = act gs move
  in m { simGameState = gs'
       , simHistory   = simHistory m ++ [gs]
       , simMoveList  = simMoveList m ++ [move]
       }

-- | Undo one move (mirrors Undo handler).
undo :: SimModel -> SimModel
undo m = case simHistory m of
  [] -> m
  _  ->
    let prev       = last (simHistory m)
        newHistory  = init (simHistory m)
    in m { simGameState = prev
         , simHistory   = newHistory
         , simMoveList  = take (length newHistory) (simMoveList m)
         }

-- | Go to a specific move index (mirrors GotoMove handler).
gotoMove :: Int -> SimModel -> SimModel
gotoMove i m =
  let allStates  = simHistory m ++ [simGameState m]
      currentIdx = length allStates - 1
  in if i >= 0 && i < currentIdx
     then m { simGameState = allStates !! i
            , simHistory   = take i allStates
            , simMoveList  = take i (simMoveList m)
            }
     else m

-- ---------------------------------------------------------------------------
-- Invariant
-- ---------------------------------------------------------------------------

checkInvariant :: SimModel -> String -> Either String ()
checkInvariant m label =
  let hLen = length (simHistory m)
      mLen = length (simMoveList m)
  in if hLen == mLen
     then Right ()
     else Left $ label ++ ": length mHistory (" ++ show hLen
               ++ ") /= length mMoveList (" ++ show mLen ++ ")"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Pick the first legal move from the current game state.
firstLegalMove :: SimModel -> Maybe MoveAction
firstLegalMove m = case getPossibleActions (simGameState m) of
  []    -> Nothing
  (a:_) -> Just a

-- | Apply N moves using the first legal move each time.
applyNMoves :: Int -> SimModel -> SimModel
applyNMoves 0 m = m
applyNMoves n m = case firstLegalMove m of
  Nothing   -> m
  Just move -> applyNMoves (n - 1) (applyMove m move)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

runTest :: String -> Either String () -> IO Bool
runTest name result = case result of
  Right () -> do
    putStrLn $ "  PASS: " ++ name
    pure True
  Left err -> do
    putStrLn $ "  FAIL: " ++ err
    pure False

main :: IO ()
main = do
  putStrLn "History invariant tests (length mHistory == length mMoveList)"
  putStrLn ""

  let m0 = initModel Brandubh

  results <- sequence
    [ -- Initial state: both empty
      runTest "initial state" $
        checkInvariant m0 "initial"

    , -- After 1 move
      runTest "after 1 move" $ do
        let m1 = applyNMoves 1 m0
        checkInvariant m1 "1 move"

    , -- After 5 moves
      runTest "after 5 moves" $ do
        let m5 = applyNMoves 5 m0
        checkInvariant m5 "5 moves"

    , -- After 1 move then undo
      runTest "1 move then undo" $ do
        let m = undo (applyNMoves 1 m0)
        checkInvariant m "1+undo"

    , -- After 5 moves then undo
      runTest "5 moves then undo" $ do
        let m = undo (applyNMoves 5 m0)
        checkInvariant m "5+undo"

    , -- After 5 moves then 3 undos
      runTest "5 moves then 3 undos" $ do
        let m = undo . undo . undo $ applyNMoves 5 m0
        checkInvariant m "5+3undo"

    , -- Undo all moves
      runTest "undo all moves" $ do
        let m = undo . undo . undo . undo . undo $ applyNMoves 5 m0
        checkInvariant m "5+5undo"

    , -- Undo on empty history (no-op)
      runTest "undo on empty history" $ do
        let m = undo m0
        checkInvariant m "undo-empty"

    , -- GotoMove 0 from 5 moves in
      runTest "goto move 0 from 5 moves" $ do
        let m = gotoMove 0 (applyNMoves 5 m0)
        checkInvariant m "goto-0"

    , -- GotoMove 2 from 5 moves in
      runTest "goto move 2 from 5 moves" $ do
        let m = gotoMove 2 (applyNMoves 5 m0)
        checkInvariant m "goto-2"

    , -- GotoMove to current position (no-op)
      runTest "goto current position (no-op)" $ do
        let m5 = applyNMoves 5 m0
            m  = gotoMove 5 m5
        checkInvariant m "goto-current"

    , -- GotoMove out of bounds (no-op)
      runTest "goto out of bounds (no-op)" $ do
        let m5 = applyNMoves 5 m0
            m  = gotoMove 99 m5
        checkInvariant m "goto-oob"

    , -- Goto then undo
      runTest "goto move 3 then undo" $ do
        let m = undo . gotoMove 3 $ applyNMoves 5 m0
        checkInvariant m "goto3+undo"

    , -- Goto then make new moves
      runTest "goto move 2 then make 2 new moves" $ do
        let m = applyNMoves 2 . gotoMove 2 $ applyNMoves 5 m0
        checkInvariant m "goto2+2new"

    , -- Repeated undo/redo cycle
      runTest "undo then make new move" $ do
        let m3 = applyNMoves 3 m0
            m2 = undo m3
            m  = applyNMoves 1 m2
        checkInvariant m "undo+new"

    , -- All variants start clean
      runTest "all variants start clean" $ do
        let variants = [minBound .. maxBound] :: [BoardVariant]
        mapM_ (\v -> checkInvariant (initModel v) (show v)) variants
    ]

  putStrLn ""
  let passed = length (filter id results)
      total  = length results
  putStrLn $ show passed ++ "/" ++ show total ++ " tests passed"

  if all id results
    then putStrLn "All tests passed."
    else do
      putStrLn "Some tests FAILED."
      error "test failure"
