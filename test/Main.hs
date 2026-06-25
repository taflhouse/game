module Main where

import Tafl.Types
import Tafl.Rules (BoardVariant(..))
import Tafl.Game (act, initialState)
import Tafl.Move (getPossibleActions)

-- ---------------------------------------------------------------------------
-- Minimal model simulation (mirrors app/Main.hs update logic)
-- ---------------------------------------------------------------------------

data GameMode = SPractice | SAi | SMultiplayer deriving (Eq, Show)

-- | The subset of Model state relevant to the history invariant.
data SimModel = SimModel
  { simGameState   :: GameState
  , simHistory     :: [GameState]
  , simMoveList    :: [MoveAction]
  , simBrowseIndex :: Maybe Int
  , simGameMode    :: GameMode
  -- Legacy fields kept for undo compatibility
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
-- Mirrors displayedGameState in app/Main.hs.
displayedState :: SimModel -> GameState
displayedState m = case simBrowseIndex m of
  Nothing -> simGameState m
  Just i  -> let allStates = simHistory m ++ [simGameState m]
             in if i >= 0 && i < length allStates
                then allStates !! i
                else simGameState m

-- | Apply a move (mirrors CellClicked handler).
-- In multiplayer, moves are blocked while browsing.
-- In single player, making a move from a browsed position forks history.
applyMove :: SimModel -> MoveAction -> SimModel
applyMove m move
  | simGameMode m == SMultiplayer && simBrowseIndex m /= Nothing = m  -- blocked
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
-- Now just sets browseIndex without mutating history.
gotoMove :: Int -> SimModel -> SimModel
gotoMove i m =
  let allStates = simHistory m ++ [simGameState m]
      lastIdx = length allStates - 1
      idx = if i >= lastIdx then Nothing else Just (max 0 i)
  in m { simBrowseIndex = idx }

-- | The true game result, accounting for history browsing.
trueGameResult :: SimModel -> GameResult
trueGameResult m = gsResult (simGameState m)

-- | Is the game truly over?
isGameOver :: SimModel -> Bool
isGameOver = finished . trueGameResult

-- | All states in history (always the full list since browse doesn't mutate).
allStates :: SimModel -> [GameState]
allStates m = simHistory m ++ [simGameState m]

-- | Is the model currently browsing (not at the latest move)?
isBrowsing :: SimModel -> Bool
isBrowsing m = simBrowseIndex m /= Nothing

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

-- | Pick the first legal move from the displayed game state.
firstLegalMove :: SimModel -> Maybe MoveAction
firstLegalMove m = case getPossibleActions (displayedState m) of
  []    -> Nothing
  (a:_) -> Just a

-- | Pick the second legal move (for forking tests).
secondLegalMove :: SimModel -> Maybe MoveAction
secondLegalMove m = case getPossibleActions (displayedState m) of
  (_:b:_) -> Just b
  _       -> Nothing

-- | Apply N moves using the first legal move each time.
applyNMoves :: Int -> SimModel -> SimModel
applyNMoves 0 m = m
applyNMoves n m = case firstLegalMove m of
  Nothing   -> m
  Just move -> applyNMoves (n - 1) (applyMove m move)

-- | Play until the game ends or a move limit is reached.
playToEnd :: Int -> SimModel -> SimModel
playToEnd 0 m = m
playToEnd n m
  | finished (gsResult (simGameState m)) = m
  | otherwise = case firstLegalMove m of
      Nothing   -> m
      Just move -> playToEnd (n - 1) (applyMove m move)

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

    , -- Goto then make new moves (single player fork)
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

    -- -----------------------------------------------------------------------
    -- Browse index tests
    -- -----------------------------------------------------------------------

    , runTest "browse does not mutate history" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
        -- History and moveList should be unchanged
        if length (simHistory m2) == length (simHistory m5)
           && length (simMoveList m2) == length (simMoveList m5)
        then Right ()
        else Left "history was mutated by gotoMove"

    , runTest "browse sets browseIndex" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
        if simBrowseIndex m2 == Just 2 then Right ()
        else Left $ "expected browseIndex Just 2, got " ++ show (simBrowseIndex m2)

    , runTest "goto last move clears browseIndex" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
            m5' = gotoMove 5 m2
        if simBrowseIndex m5' == Nothing then Right ()
        else Left "browseIndex not cleared when going to last move"

    , runTest "displayedState shows browsed state" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
            states = allStates m5
        if displayedState m2 == states !! 2 then Right ()
        else Left "displayedState does not match browsed index"

    , runTest "displayedState shows current when not browsing" $ do
        let m5 = applyNMoves 5 m0
        if displayedState m5 == simGameState m5 then Right ()
        else Left "displayedState should equal simGameState when not browsing"

    , runTest "all states preserved while browsing" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
            total = length (allStates m2)
        if total == 6 then Right ()  -- 5 moves + initial = 6 states
        else Left $ "expected 6 states, got " ++ show total

    -- -----------------------------------------------------------------------
    -- Single player: browse and fork
    -- -----------------------------------------------------------------------

    , runTest "SP: can make move while browsing (fork)" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
            m3 = applyNMoves 1 m2
        -- Should have 3 states in history (indices 0,1,2) + current = 4 total
        if length (allStates m3) == 4
        then checkInvariant m3 "sp-fork"
        else Left $ "expected 4 states after fork, got " ++ show (length (allStates m3))

    , runTest "SP: fork clears browseIndex" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
            m3 = applyNMoves 1 m2
        if simBrowseIndex m3 == Nothing then Right ()
        else Left "browseIndex not cleared after fork"

    , runTest "SP: fork truncates future moves" $ do
        let m5 = applyNMoves 5 m0
            m2 = gotoMove 2 m5
            m3 = applyNMoves 1 m2
        -- Move list should have 3 entries (2 kept + 1 new)
        if length (simMoveList m3) == 3 then Right ()
        else Left $ "expected 3 moves after fork, got " ++ show (length (simMoveList m3))

    , runTest "SP: fork from move 0 replaces all history" $ do
        let m5 = applyNMoves 5 m0
            m0' = gotoMove 0 m5
            m1  = applyNMoves 1 m0'
        if length (simMoveList m1) == 1
           && length (simHistory m1) == 1
        then checkInvariant m1 "sp-fork-from-0"
        else Left $ "expected 1 move after fork from 0, got " ++ show (length (simMoveList m1))

    , runTest "SP: multiple forks maintain invariant" $ do
        let m5  = applyNMoves 5 m0
            m2  = gotoMove 2 m5
            m3  = applyNMoves 1 m2       -- fork at 2
            m1' = gotoMove 1 m3
            m2' = applyNMoves 1 m1'      -- fork at 1
        checkInvariant m2' "sp-double-fork"

    -- -----------------------------------------------------------------------
    -- Multiplayer: browse but no fork
    -- -----------------------------------------------------------------------

    , runTest "MP: can browse history" $ do
        let m0mp = initModelWithMode Brandubh SMultiplayer
            m5 = applyNMoves 5 m0mp
            m2 = gotoMove 2 m5
        if isBrowsing m2 then Right ()
        else Left "should be browsing after gotoMove in multiplayer"

    , runTest "MP: browse does not mutate history" $ do
        let m0mp = initModelWithMode Brandubh SMultiplayer
            m5 = applyNMoves 5 m0mp
            m2 = gotoMove 2 m5
        if length (simHistory m2) == length (simHistory m5)
           && length (simMoveList m2) == length (simMoveList m5)
        then Right ()
        else Left "history was mutated by browse in multiplayer"

    , runTest "MP: move blocked while browsing" $ do
        let m0mp = initModelWithMode Brandubh SMultiplayer
            m5 = applyNMoves 5 m0mp
            m2 = gotoMove 2 m5
            m2' = applyNMoves 1 m2  -- should be no-op
        -- State should be identical to m2 (move was blocked)
        if simBrowseIndex m2' == simBrowseIndex m2
           && length (simMoveList m2') == length (simMoveList m2)
        then Right ()
        else Left "move was not blocked while browsing in multiplayer"

    , runTest "MP: can return to latest after browsing" $ do
        let m0mp = initModelWithMode Brandubh SMultiplayer
            m5 = applyNMoves 5 m0mp
            m2 = gotoMove 2 m5
            m5' = gotoMove 5 m2
        if not (isBrowsing m5') then Right ()
        else Left "should not be browsing after going to latest"

    , runTest "MP: history unchanged after browse round-trip" $ do
        let m0mp = initModelWithMode Brandubh SMultiplayer
            m5 = applyNMoves 5 m0mp
            m2 = gotoMove 2 m5
            m5' = gotoMove 5 m2
        if simGameState m5' == simGameState m5
           && length (simHistory m5') == length (simHistory m5)
           && length (simMoveList m5') == length (simMoveList m5)
        then Right ()
        else Left "state changed after browse round-trip in multiplayer"

    -- -----------------------------------------------------------------------
    -- Game-over invariant
    -- -----------------------------------------------------------------------

    , runTest "game over persists when browsing back" $ do
        let mDone = playToEnd 500 m0
        if not (isGameOver mDone) then Right ()
        else let m2 = gotoMove 2 mDone
             in if isGameOver m2 then Right ()
                else Left "game no longer over after browsing back"

    , runTest "undo on finished game reverts to non-finished" $ do
        let mDone = playToEnd 500 m0
        if not (isGameOver mDone) then Right ()
        else let m' = undo mDone
             in if not (isGameOver m') then Right ()
                else Left "game should not be over after undo"

    , runTest "game over persists after multiple gotos" $ do
        let mDone = playToEnd 500 m0
        if not (isGameOver mDone) then Right ()
        else let m0' = gotoMove 0 mDone
                 m1' = gotoMove 1 m0'
             in if isGameOver m1' then Right ()
                else Left "game no longer over after multiple gotos"
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
