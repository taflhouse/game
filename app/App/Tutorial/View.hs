{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.View (viewTutorial) where

import Miso hiding ((!!))
import Miso.String (MisoString, ms)
import Miso.CSS (style_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Tafl.Board
import Tafl.Game.State (GameState(..))

import App.Board (sqSize, viewBasicSVGBoard, viewBoardContainer, viewEvalBar, renderPiece, renderCapturePoofs, svgDefs, renderSquareBg, renderSpecialSquares)
import App.Tutorial.Model
import App.Tutorial.Action
import App.Tutorial.Lessons

-- ---------------------------------------------------------------------------
-- Top-level view
-- ---------------------------------------------------------------------------

viewTutorial :: TutorialProps -> TutorialModel -> View TutorialModel TutorialAction
viewTutorial props m = case tmLesson m of
  Nothing -> viewLessonSelect m
  Just lesson -> viewActiveLesson lesson m

-- ---------------------------------------------------------------------------
-- Lesson select screen
-- ---------------------------------------------------------------------------

viewLessonSelect :: TutorialModel -> View TutorialModel TutorialAction
viewLessonSelect m =
  let totalLessons = length allLessons
      completedCount = length (filter (\l -> tlId l `elem` tmCompletedLessons m) allLessons)
      allComplete = completedCount >= totalLessons
  in if allComplete
       then viewBrowseAll m
       else viewWizard m completedCount totalLessons

-- ---------------------------------------------------------------------------
-- Wizard mode (typeform-style: one lesson at a time)
-- ---------------------------------------------------------------------------

viewWizard :: TutorialModel -> Int -> Int -> View TutorialModel TutorialAction
viewWizard m completedCount totalLessons =
  let nextLesson = case filter (\l -> tlId l `notElem` tmCompletedLessons m) allLessons of
        (l:_) -> l
        []    -> case allLessons of { (l:_) -> l; [] -> error "no lessons" }
  in H.div_
    [ HP.class_ "w-full flex flex-col items-center" ]
    [ H.div_
        [ HP.class_ "w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ viewProgressBar m completedCount totalLessons
        , viewJumpTo m
        , viewBigLessonCard nextLesson
        ]
    ]

viewProgressBar :: TutorialModel -> Int -> Int -> View TutorialModel TutorialAction
viewProgressBar m completedCount totalLessons =
  let nextId = case filter (\l -> tlId l `notElem` tmCompletedLessons m) allLessons of
        (l:_) -> tlId l
        []    -> ""
  in H.div_
    [ HP.class_ "mb-4" ]
    [ H.div_
        [ HP.class_ "flex gap-1"
        , style_ [("margin-bottom", "0.5em")]
        ]
        (map (\l ->
          let lid = tlId l
              isCompleted = lid `elem` tmCompletedLessons m
              isCurrent = lid == nextId
              bgColor | isCompleted = "var(--primary)"
                      | isCurrent   = "color-mix(in oklch, var(--primary) 50%, transparent)"
                      | otherwise   = "var(--muted)"
              extraClass = if isCurrent then " tutorial-pulse" else ""
          in H.div_
            [ HP.class_ ("rounded-sm flex-1" <> extraClass)
            , style_ [ ("height", "6px")
                     , ("background", bgColor)
                     ]
            ]
            []
        ) allLessons)
    , H.p_
        [ HP.class_ "text-xs text-muted-foreground" ]
        [ text (ms (show completedCount) <> " of " <> ms (show totalLessons) <> " lessons complete") ]
    ]

viewJumpTo :: TutorialModel -> View TutorialModel TutorialAction
viewJumpTo m =
  let completed = filter (\l -> tlId l `elem` tmCompletedLessons m) allLessons
  in if null completed
       then text ""
       else H.div_
         [ HP.class_ "mb-4" ]
         [ H.select_
             [ HP.class_ "w-full rounded-md border border-border bg-card text-foreground text-sm p-2"
             , H.onChange TSelectLesson
             ]
             ( H.option_
                 [ HP.value_ ""
                 , boolProp "disabled" True
                 , boolProp "selected" True
                 ]
                 [ text "Jump to completed lesson\x2026" ]
             : map (\l -> H.option_
                 [ HP.value_ (tlId l) ]
                 [ text ("\x2713 " <> tlTitle l) ]
               ) completed
             )
         ]

viewBigLessonCard :: TutorialLesson -> View TutorialModel TutorialAction
viewBigLessonCard lesson =
  H.div_
    [ HP.class_ "rounded-xl border border-primary bg-card p-6 shadow-lg cursor-pointer transition-transform hover:scale-[1.02] active:scale-[0.98]"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (TSelectLesson (tlId lesson))
    ]
    [ H.div_
        [ HP.class_ "text-xs font-semibold text-muted-foreground uppercase tracking-wider mb-2" ]
        [ text (moduleLabel (tlModule lesson)) ]
    , H.h2_
        [ HP.class_ "text-xl font-bold text-foreground mb-2" ]
        [ text (tlTitle lesson) ]
    , H.button_
        [ HP.class_ "btn bg-primary text-primary-foreground w-full"
        , style_ [("touch-action", "manipulation")]
        , SVG.onClick (TSelectLesson (tlId lesson))
        ]
        [ text "Start" ]
    ]

moduleLabel :: TutorialModule -> MisoString
moduleLabel BeginnerModule     = "Beginner"
moduleLabel IntermediateModule = "Intermediate"
moduleLabel AdvancedModule     = "Advanced"

-- ---------------------------------------------------------------------------
-- Browse mode (all lessons completed — full list)
-- ---------------------------------------------------------------------------

viewBrowseAll :: TutorialModel -> View TutorialModel TutorialAction
viewBrowseAll m =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center" ]
    [ H.div_
        [ HP.class_ "w-full max-w-sm" ]
        [ H.h1_
            [ HP.class_ "text-2xl font-bold text-foreground mb-2"
            , style_ [("margin-top", "4em")]
            ]
            [ text "Learn Tafl" ]
        , H.p_
            [ HP.class_ "text-sm text-muted-foreground mb-6" ]
            [ text "All lessons complete! Revisit any lesson below." ]
        , viewModuleSection "Beginner" BeginnerModule m
        , viewModuleSection "Intermediate" IntermediateModule m
        , viewModuleSection "Advanced" AdvancedModule m
        ]
    ]

viewModuleSection :: MisoString -> TutorialModule -> TutorialModel -> View TutorialModel TutorialAction
viewModuleSection title mod' m =
  let lessons = moduleLessons mod'
  in H.div_
    [ HP.class_ "mb-6"
    , style_ [("margin-bottom", "2em")]
    ]
    [ H.div_
        [ HP.class_ "text-xs font-semibold text-muted-foreground uppercase tracking-wider"
        , style_ [("margin-bottom", "0.75rem"), ("margin-top", "0.5rem")]
        ]
        [ text title ]
    , H.div_
        [ HP.class_ "flex flex-col gap-2" ]
        (map (viewLessonCard m) lessons)
    ]

viewLessonCard :: TutorialModel -> TutorialLesson -> View TutorialModel TutorialAction
viewLessonCard m lesson =
  let completed = tlId lesson `elem` tmCompletedLessons m
      -- Find first uncompleted lesson
      firstUncompleted = case filter (\l -> tlId l `notElem` tmCompletedLessons m) allLessons of
        (l:_) -> tlId l
        []    -> ""
      isNext = tlId lesson == firstUncompleted
  in H.div_
    [ HP.class_ ("rounded-lg border cursor-pointer transition-colors bg-card hover:bg-accent active:bg-accent/80 focus:bg-accent"
        <> if isNext then " border-primary" else " border-border")
    , style_ [("touch-action", "manipulation"), ("padding", "0.875rem 1rem")]
    , SVG.onClick (TSelectLesson (tlId lesson))
    ]
    [ H.div_
        [ HP.class_ "flex justify-between items-center" ]
        [ H.span_
            [ HP.class_ "font-medium text-foreground" ]
            [ text (tlTitle lesson) ]
        , if completed
            then H.span_
              [ HP.class_ "text-green-500 text-sm" ]
              [ text "\x2713" ]
            else text ""
        ]
    ]

-- ---------------------------------------------------------------------------
-- Active lesson view
-- ---------------------------------------------------------------------------

viewActiveLesson :: TutorialLesson -> TutorialModel -> View TutorialModel TutorialAction
viewActiveLesson lesson m =
  let step = tlSteps lesson !! tmStepIndex m
      gs = tmGameState m
      n = boardSize (gsBoard gs)
  in H.div_
    [ HP.class_ "flex flex-col items-center w-full" ]
    [ -- Back to lessons link
      H.div_
        [ HP.class_ "w-full max-w-2xl mb-4"
        , style_ [("margin-top", "2em")]
        ]
        [ H.span_
            [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick TBackToLessons
            ]
            [ text "\x2190 All Lessons" ]
        ]
    , -- Title
      H.h2_
        [ HP.class_ "text-lg font-bold text-foreground mb-4" ]
        [ text (tlTitle lesson) ]
    , -- Board + optional eval bar
      H.div_
        [ HP.class_ "flex items-stretch gap-2" ]
        [ if tlShowEvalBar lesson
            then viewEvalBar (tmEvalScore m)
            else text ""
        , viewBoardContainer False False n
            (viewTutorialBoard gs step m)
        ]
    , -- Instruction card
      viewInstructionCard lesson step m
    , -- Congrats overlay
      if tmShowCongrats m then viewCongratsOverlay lesson else text ""
    ]

-- ---------------------------------------------------------------------------
-- Tutorial board (reuses App.Board primitives with overlays)
-- ---------------------------------------------------------------------------

viewTutorialBoard :: GameState -> TutorialStep -> TutorialModel -> View TutorialModel TutorialAction
viewTutorialBoard gs step m =
  let board = gsBoard gs
      n = boardSize board
      total = sqSize * n
      pieceK r c = "p-" <> ms r <> "-" <> ms c

      -- Determine which pieces should be dimmed
      dimmedPieces = case tsKind step of
        MoveStep (Just allowed) _ _ ->
          [ (r, c) | r <- [0..n-1], c <- [0..n-1]
          , let p = pieceAt board (Coords r c)
          , p /= Empty
          , Coords r c `notElem` allowed
          , canControl (tsPlayerSide step) p
          ]
        _ -> []

      -- Highlight squares
      highlights = tsHighlightSquares step

      -- Valid move dots
      validDots = tmValidMoves m

      -- Selection highlight
      selectedSq = tmSelected m

  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , HP.class_ "block aspect-square"
    ]
    ( svgDefs
    -- Board squares
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    -- Special squares
    ++ renderSpecialSquares gs n
    -- Highlight squares (pulse/glow)
    ++ map (renderHighlight (tsPlayerSide step)) highlights
    -- Selection highlight
    ++ maybe [] (\sel -> [renderSelection sel]) selectedSq
    -- Last move (green origin, piece-colored destination)
    ++ renderTutorialLastMove gs
    -- Pieces (with dimming)
    ++ [ SVG.g_
         (if (r, c) `elem` dimmedPieces
          then [style_ [("opacity", "0.3")]]
          else [])
         [ renderPiece (pieceK r c) True r c (pieceAt board (Coords r c)) ]
       | r <- [0..n-1], c <- [0..n-1]
       , pieceAt board (Coords r c) /= Empty
       ]
    -- Valid move dots
    ++ map renderValidDot validDots
    -- Capture poofs
    ++ renderCapturePoofs (tmCapturePoofs m)
    -- Click targets (invisible rects for the whole board)
    ++ [ SVG.rect_
          [ SP.x_ (ms (c * sqSize))
          , SP.y_ (ms (r * sqSize))
          , HP.width_ (ms sqSize)
          , HP.height_ (ms sqSize)
          , SP.fill_ "transparent"
          , SVG.onClick (TCellClicked (Coords r c))
          , style_ [("cursor", "pointer")]
          ]
       | r <- [0..n-1], c <- [0..n-1]
       ]
    )

renderHighlight :: Side -> HighlightSquare -> View TutorialModel TutorialAction
renderHighlight _side (HighlightSquare (Coords r c) style') =
  let opacity = case style' of
        PulseHighlight -> "35%"
        GlowHighlight  -> "45%"
      color = "color-mix(in oklch, #22c55e " <> opacity <> ", transparent)"
      animClass = case style' of
        PulseHighlight -> "tutorial-pulse"
        GlowHighlight  -> "tutorial-glow"
  in SVG.rect_
    [ SP.x_ (ms (c * sqSize + 1))
    , SP.y_ (ms (r * sqSize + 1))
    , HP.width_ (ms (sqSize - 2))
    , HP.height_ (ms (sqSize - 2))
    , SP.fill_ color
    , SP.rx_ "3"
    , HP.class_ animClass
    ]

-- | Tutorial last move: piece-colored for both origin and destination
-- (same as regular game). Green highlights come from renderHighlight.
renderTutorialLastMove :: GameState -> [View TutorialModel TutorialAction]
renderTutorialLastMove gs = case gsLastAction gs of
  Nothing -> []
  Just (MoveAction f t) ->
    let movedPiece = pieceAt (gsBoard gs) t
        hlColor = case movedPiece of
          King     -> "color-mix(in oklch, var(--piece-king) 40%, transparent)"
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 30%, transparent)"
          _        -> "color-mix(in oklch, var(--piece-defender) 50%, transparent)"
        mkRect (Coords r c) = SVG.rect_
          [ SP.x_ (ms (c * sqSize))
          , SP.y_ (ms (r * sqSize))
          , HP.width_ (ms sqSize)
          , HP.height_ (ms sqSize)
          , SP.fill_ hlColor
          ]
    in [ mkRect f, mkRect t ]

renderSelection :: Coords -> View TutorialModel TutorialAction
renderSelection (Coords r c) =
  SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ "rgba(59, 130, 246, 0.25)"
    ]

renderValidDot :: Coords -> View TutorialModel TutorialAction
renderValidDot (Coords r c) =
  let cx = c * sqSize + sqSize `div` 2
      cy = r * sqSize + sqSize `div` 2
  in SVG.circle_
    [ SP.cx_ (ms cx)
    , SP.cy_ (ms cy)
    , SP.r_ "6"
    , SP.fill_ "rgba(59, 130, 246, 0.5)"
    ]

-- ---------------------------------------------------------------------------
-- Instruction card
-- ---------------------------------------------------------------------------

viewInstructionCard :: TutorialLesson -> TutorialStep -> TutorialModel -> View TutorialModel TutorialAction
viewInstructionCard lesson step m =
  let totalSteps = length (tlSteps lesson)
      currentStep = tmStepIndex m + 1
      isInfoStep = case tsKind step of
        InfoStep -> True
        _        -> False
  in H.div_
    [ HP.class_ "w-full mt-4"
    , style_ [("max-width", ms (sqSize * boardSize (gsBoard (tmGameState m))) <> "px")]
    ]
    [ -- Instruction text
      H.div_
        [ HP.class_ "card p-4" ]
        [ H.p_
            [ HP.class_ "text-sm text-foreground" ]
            [ text (tsInstruction step) ]
        , case tsDetail step of
            Just detail ->
              H.p_
                [ HP.class_ "text-xs text-muted-foreground mt-2" ]
                [ text detail ]
            Nothing -> text ""
        , -- Hint (shown after failures)
          if tmShowHint m
            then case tsHint step of
              Just hint ->
                H.p_
                  [ HP.class_ "text-xs mt-2 italic"
                  , style_ [("color", "var(--piece-king)")]
                  ]
                  [ text hint ]
              Nothing -> text ""
            else text ""
        , -- Buttons
          H.div_
            [ HP.class_ "flex items-center justify-between mt-4" ]
            [ H.div_
                [ HP.class_ "flex gap-2" ]
                [ if tmStepIndex m > 0
                    then H.button_
                      [ HP.class_ "btn btn-outline btn-sm text-foreground"
                      , style_ [("touch-action", "manipulation")]
                      , SVG.onClick TBackStep
                      ]
                      [ text "Back" ]
                    else text ""
                , if isInfoStep
                    then H.button_
                      [ HP.class_ "btn btn-sm bg-primary text-primary-foreground"
                      , style_ [("touch-action", "manipulation")]
                      , SVG.onClick TNextStep
                      ]
                      [ text "Next" ]
                    else text ""
                ]
            , -- Step counter
              H.span_
                [ HP.class_ "text-xs text-muted-foreground" ]
                [ text ("Step " <> ms (show currentStep) <> " of " <> ms (show totalSteps)) ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Congrats overlay
-- ---------------------------------------------------------------------------

viewCongratsOverlay :: TutorialLesson -> View TutorialModel TutorialAction
viewCongratsOverlay lesson =
  let isLastLesson = tlId lesson == tlId (last allLessons)
  in H.div_ []
    [ H.div_
        [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "9998")
                 , ("background", "rgba(0,0,0,0.5)")
                 ]
        , SVG.onClick TDismissCongrats
        ] []
    , H.div_
        [ HP.class_ "card p-6 shadow-xl"
        , style_ [ ("position", "fixed"), ("top", "50%"), ("left", "50%")
                 , ("transform", "translate(-50%, -50%)"), ("z-index", "9999")
                 , ("min-width", "16rem"), ("max-width", "22rem")
                 , ("text-align", "center")
                 ]
        ]
        (if isLastLesson
          then
            [ H.h3_
                [ HP.class_ "text-lg font-bold mb-2" ]
                [ text "Tutorial Complete!" ]
            , H.p_
                [ HP.class_ "text-sm text-muted-foreground mb-4" ]
                [ text "You've finished all the lessons. Time to play!" ]
            , H.div_
                [ HP.class_ "flex gap-2 justify-center" ]
                [ H.button_
                    [ HP.class_ "btn btn-outline btn-sm text-foreground"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick TBackToLessons
                    ]
                    [ text "All Lessons" ]
                , H.a_
                    [ HP.class_ "btn btn-sm bg-primary text-primary-foreground"
                    , style_ [("touch-action", "manipulation")]
                    , HP.href_ "/new-game"
                    ]
                    [ text "Play Now" ]
                ]
            ]
          else
            [ H.h3_
                [ HP.class_ "text-lg font-bold mb-2" ]
                [ text "Lesson Complete!" ]
            , H.p_
                [ HP.class_ "text-sm text-muted-foreground mb-4" ]
                [ text ("You've completed \"" <> tlTitle lesson <> "\".") ]
            , H.div_
                [ HP.class_ "flex gap-2 justify-center" ]
                [ H.button_
                    [ HP.class_ "btn btn-outline btn-sm text-foreground"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick TBackToLessons
                    ]
                    [ text "All Lessons" ]
                , H.button_
                    [ HP.class_ "btn btn-sm bg-primary text-primary-foreground"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick (nextLessonAction lesson)
                    ]
                    [ text "Next Lesson" ]
                ]
            ]
        )
    ]

-- | Get the action to navigate to the next lesson, or back to lesson list.
nextLessonAction :: TutorialLesson -> TutorialAction
nextLessonAction lesson =
  let allIds = map tlId allLessons
      go [] = TBackToLessons
      go [_] = TBackToLessons
      go (x:y:xs)
        | x == tlId lesson = TSelectLesson y
        | otherwise        = go (y:xs)
  in go allIds
