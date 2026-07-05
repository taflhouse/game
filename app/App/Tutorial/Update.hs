{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module App.Tutorial.Update (updateTutorial) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Miso hiding ((!!))
import Miso.String (MisoString, ms, fromMisoString)

import Tafl.Board
import Tafl.Game (act, initialState)
import Tafl.Game.State (GameState(..), turnSide)
import Tafl.Game.Move (getPossibleMovesFrom)
import Tafl.AI (evaluate)

import App.FFI (js_playMoveSound, js_getLocalStorage, js_setLocalStorage)
import App.Model (Model)
import App.Route (learnURI, learnLessonURI)
import App.Tutorial.Model
import App.Tutorial.Action
import App.Tutorial.Lessons

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateTutorial :: TutorialAction -> Effect Model TutorialProps TutorialModel TutorialAction
updateTutorial = \case
  TutorialMount -> do
    -- Load progress from localStorage
    withSink $ \sink -> do
      raw <- js_getLocalStorage "taflhouse_tutorial_progress"
      let ids = parseProgressList raw
      sink (TLoadProgress ids)
    -- If props specify a lesson, select it
    props <- getProps
    case tpLessonId props of
      Just lid -> updateTutorial (TSelectLesson lid)
      Nothing  -> pure ()

  TutorialUnmount -> pure ()

  TLoadProgress ids ->
    modify $ \m -> m { tmCompletedLessons = ids }

  TSelectLesson lid -> do
    case lookupLesson lid of
      Nothing -> modify $ \m -> m { tmLessonNotFound = True }
      Just lesson -> do
        let gs0 = buildGameState lesson
        modify $ \m -> m
          { tmLesson        = Just lesson
          , tmStepIndex     = 0
          , tmGameState     = gs0
          , tmSelected      = Nothing
          , tmValidMoves    = []
          , tmAnimateMove   = Nothing
          , tmCapturePoofs  = []
          , tmEvalScore     = evaluate gs0
          , tmShowCongrats  = False
          , tmStepComplete  = False
          , tmFailureCount  = 0
          , tmShowHint      = False
          , tmLessonNotFound = False
          , tmStateHistory  = [gs0]
          }
        io_ $ pushURI (learnLessonURI lid)

  TBackToLessons -> do
    modify $ \m -> m
      { tmLesson       = Nothing
      , tmStepIndex    = 0
      , tmSelected     = Nothing
      , tmValidMoves   = []
      , tmShowCongrats = False
      , tmStepComplete = False
      , tmLessonNotFound = False
      }
    io_ $ pushURI learnURI

  TNextStep -> do
    m <- get
    case tmLesson m of
      Nothing -> pure ()
      Just lesson ->
        let nextIdx = tmStepIndex m + 1
            totalSteps = length (tlSteps lesson)
        in if nextIdx >= totalSteps
          then do
            -- Lesson complete
            let lid = tlId lesson
                completed = if lid `elem` tmCompletedLessons m
                            then tmCompletedLessons m
                            else lid : tmCompletedLessons m
            modify $ \x -> x
              { tmShowCongrats    = True
              , tmCompletedLessons = completed
              }
            saveProgress completed
          else do
            let step = tlSteps lesson !! nextIdx
                gs = syncTurn (tsPlayerSide step) (tmGameState m)
                history = take nextIdx (tmStateHistory m) ++ [gs]
            modify $ \x -> x
              { tmStepIndex    = nextIdx
              , tmGameState    = gs
              , tmSelected     = Nothing
              , tmValidMoves   = []
              , tmStepComplete  = False
              , tmFailureCount  = 0
              , tmShowHint      = False
              , tmEvalScore     = evaluate gs
              , tmStateHistory  = history
              , tmCapturePoofs  = []
              }

  TBackStep -> do
    m <- get
    case tmLesson m of
      Nothing -> pure ()
      Just _lesson -> do
        let prevIdx = max 0 (tmStepIndex m - 1)
            gs0 = case drop prevIdx (tmStateHistory m) of
              (s:_) -> s
              []    -> tmGameState m
        modify $ \x -> x
          { tmStepIndex    = prevIdx
          , tmGameState    = gs0
          , tmSelected     = Nothing
          , tmValidMoves   = []
          , tmStepComplete = False
          , tmFailureCount = 0
          , tmShowHint      = False
          , tmEvalScore     = evaluate gs0
          , tmCapturePoofs  = []
          }

  TCellClicked coords -> do
    m <- get
    case tmLesson m of
      Nothing -> pure ()
      Just lesson -> do
        let step = tlSteps lesson !! tmStepIndex m
            gs = tmGameState m
        when (not (tmStepComplete m)) $
          case tsKind step of
            InfoStep -> pure ()

            MoveStep mAllowedPieces mAllowedTargets mAutoResp ->
              handleMoveStepClick coords gs step mAllowedPieces mAllowedTargets mAutoResp

            ChallengeStep predicate mAutoResp ->
              handleChallengeClick coords gs step predicate mAutoResp

  TAdvanceAfterDelay -> do
    m <- get
    when (tmStepComplete m) $
      updateTutorial TNextStep

  TAutoResponseExec autoMove -> do
    m <- get
    let gs = tmGameState m
        gs' = act gs autoMove
        poofs = [(c, pieceAt (gsBoard gs) c) | c <- gsCaptures gs']
    io_ js_playMoveSound
    modify $ \x -> x
      { tmGameState   = gs'
      , tmAnimateMove  = Just autoMove
      , tmEvalScore    = evaluate gs'
      , tmCapturePoofs = poofs
      }
    when (not (null poofs)) $
      withSink $ \sink -> do
        threadDelay 400000
        sink TPoofsDone
    withSink $ \sink -> do
      threadDelay 400000
      sink TAdvanceAfterDelay

  TAutoResponseDone -> pure ()

  TPoofsDone ->
    modify $ \m -> m { tmCapturePoofs = [] }

  TDismissCongrats ->
    modify $ \m -> m { tmShowCongrats = False }

-- ---------------------------------------------------------------------------
-- Move step click handler
-- ---------------------------------------------------------------------------

handleMoveStepClick
  :: Coords -> GameState -> TutorialStep
  -> Maybe [Coords] -> Maybe [Coords] -> Maybe MoveAction
  -> Effect Model TutorialProps TutorialModel TutorialAction
handleMoveStepClick coords gs step mAllowedPieces mAllowedTargets mAutoResp = do
  m <- get
  let board = gsBoard gs
      piece = pieceAt board coords
      side  = tsPlayerSide step

  case tmSelected m of
    Nothing ->
      -- Trying to select a piece
      if canControl side piece && pieceAllowed coords mAllowedPieces
        then do
          let allMoves = getPossibleMovesFrom gs coords
              validMoves = case mAllowedTargets of
                Nothing      -> allMoves
                Just targets -> filter (`elem` targets) allMoves
          modify $ \x -> x
            { tmSelected  = Just coords
            , tmValidMoves = validMoves
            }
        else
          when (piece /= Empty) $
            modify $ \x -> x
              { tmFailureCount = tmFailureCount x + 1
              , tmShowHint     = tmFailureCount x + 1 >= 2 && tsHint step /= Nothing
              }

    Just sel ->
      if coords == sel
        then
          modify $ \x -> x { tmSelected = Nothing, tmValidMoves = [] }
        else if coords `elem` tmValidMoves m
          then do
            let moveAction = MoveAction sel coords
                gs' = act gs moveAction
                poofs = [(c, pieceAt (gsBoard gs) c) | c <- gsCaptures gs']
            io_ js_playMoveSound
            modify $ \x -> x
              { tmGameState   = gs'
              , tmSelected    = Nothing
              , tmValidMoves  = []
              , tmAnimateMove = Just moveAction
              , tmEvalScore   = evaluate gs'
              , tmStepComplete = True
              , tmFailureCount = 0
              , tmShowHint     = False
              , tmCapturePoofs = poofs
              }
            when (not (null poofs)) $
              withSink $ \sink -> do
                threadDelay 400000
                sink TPoofsDone
            case mAutoResp of
              Just autoMove -> withSink $ \sink -> do
                threadDelay 600000
                sink (TAutoResponseExec autoMove)
              Nothing -> withSink $ \sink -> do
                threadDelay 800000
                sink TAdvanceAfterDelay
          else if canControl side (pieceAt board coords) && pieceAllowed coords mAllowedPieces
            then do
              let allMoves = getPossibleMovesFrom gs coords
                  validMoves = case mAllowedTargets of
                    Nothing      -> allMoves
                    Just targets -> filter (`elem` targets) allMoves
              modify $ \x -> x
                { tmSelected  = Just coords
                , tmValidMoves = validMoves
                }
            else
              modify $ \x -> x
                { tmSelected     = Nothing
                , tmValidMoves   = []
                , tmFailureCount = tmFailureCount x + 1
                , tmShowHint     = tmFailureCount x + 1 >= 2 && tsHint step /= Nothing
                }

-- ---------------------------------------------------------------------------
-- Challenge step click handler
-- ---------------------------------------------------------------------------

handleChallengeClick
  :: Coords -> GameState -> TutorialStep
  -> (GameState -> Bool) -> Maybe MoveAction
  -> Effect Model TutorialProps TutorialModel TutorialAction
handleChallengeClick coords gs step predicate mAutoResp = do
  m <- get
  let board = gsBoard gs
      piece = pieceAt board coords
      side  = tsPlayerSide step

  case tmSelected m of
    Nothing ->
      when (canControl side piece) $ do
        let validMoves = getPossibleMovesFrom gs coords
        modify $ \x -> x
          { tmSelected  = Just coords
          , tmValidMoves = validMoves
          }

    Just sel ->
      if coords == sel
        then modify $ \x -> x { tmSelected = Nothing, tmValidMoves = [] }
        else if coords `elem` tmValidMoves m
          then do
            let moveAction = MoveAction sel coords
                gs' = act gs moveAction
            if predicate gs'
              then do
                let poofs = [(c, pieceAt (gsBoard gs) c) | c <- gsCaptures gs']
                io_ js_playMoveSound
                modify $ \x -> x
                  { tmGameState   = gs'
                  , tmSelected    = Nothing
                  , tmValidMoves  = []
                  , tmAnimateMove = Just moveAction
                  , tmEvalScore   = evaluate gs'
                  , tmStepComplete = True
                  , tmFailureCount = 0
                  , tmShowHint     = False
                  , tmCapturePoofs = poofs
                  }
                when (not (null poofs)) $
                  withSink $ \sink -> do
                    threadDelay 400000
                    sink TPoofsDone
                case mAutoResp of
                  Just autoMove -> withSink $ \sink -> do
                    threadDelay 600000
                    sink (TAutoResponseExec autoMove)
                  Nothing -> withSink $ \sink -> do
                    threadDelay 800000
                    sink TAdvanceAfterDelay
              else
                -- Failed: don't apply move, show hint immediately
                modify $ \x -> x
                  { tmSelected     = Nothing
                  , tmValidMoves   = []
                  , tmFailureCount = tmFailureCount x + 1
                  , tmShowHint     = tsHint step /= Nothing
                  }
          else if canControl side (pieceAt board coords)
            then do
              let validMoves = getPossibleMovesFrom gs coords
              modify $ \x -> x
                { tmSelected  = Just coords
                , tmValidMoves = validMoves
                }
            else
              modify $ \x -> x { tmSelected = Nothing, tmValidMoves = [] }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

buildGameState :: TutorialLesson -> GameState
buildGameState lesson =
  let gs = initialState (tlVariant lesson)
  in gs { gsBoard = tlInitialBoard lesson
        , gsTurn  = tlInitialTurn lesson
        }

syncTurn :: Side -> GameState -> GameState
syncTurn side gs
  | turnSide gs == side = gs
  | otherwise           = gs { gsTurn = gsTurn gs + 1 }

pieceAllowed :: Coords -> Maybe [Coords] -> Bool
pieceAllowed _ Nothing          = True
pieceAllowed c (Just allowList) = c `elem` allowList

parseProgressList :: MisoString -> [MisoString]
parseProgressList s
  | s == ""   = []
  | otherwise =
      let str = fromMisoString s :: String
          go [] acc = [reverse acc]
          go (c:cs) acc
            | c == ','  = reverse acc : go cs []
            | otherwise = go cs (c : acc)
      in map ms (filter (not . null) (go str []))

saveProgress :: [MisoString] -> Effect Model TutorialProps TutorialModel TutorialAction
saveProgress ids = io_ $ do
  let str = joinWith "," ids
  js_setLocalStorage "taflhouse_tutorial_progress" str
  where
    joinWith :: MisoString -> [MisoString] -> MisoString
    joinWith _ []     = ""
    joinWith _ [x]    = x
    joinWith sep (x:xs) = x <> sep <> joinWith sep xs
