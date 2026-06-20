{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (threadDelay)
import Miso hiding ((!!))
import qualified Miso.CSS as CSS
import Miso.CSS.Color
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Tafl.Types
import Tafl.Rules (BoardVariant(..), variantDefaultRules)
import Tafl.Board (variantBoard)
import Tafl.Move  (getPossibleMovesFrom, getPossibleActions, isActionPossible)
import Tafl.Game  (act, isGameOver, initialState)
import Tafl.AI    (AiConfig(..), bestMove)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data GameMode = LocalMode | AiMode
  deriving (Eq, Show)

data Screen = SetupScreen | GameScreen
  deriving (Eq, Show)

data Model = Model
  { mScreen      :: !Screen
  , mGameMode    :: !GameMode
  , mGameState   :: !GameState
  , mSelected    :: Maybe Coords
  , mValidMoves  :: [Coords]
  , mVariant     :: !BoardVariant
  , mAiSide      :: !Side
  , mAiThinking  :: !Bool
  , mAiDepth     :: !Int
  , mAiNodeLimit :: !Int
  , mHistory     :: [GameState]
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Action
-- ---------------------------------------------------------------------------

data Action
  = CellClicked Coords
  | NewGame BoardVariant
  | NoOp
  | AiMoveComplete MoveAction
  | SetGameMode GameMode
  | SetVariant BoardVariant
  | SetAiSide Side
  | SetAiDepth Int
  | SetAiNodeLimit Int
  | GotoMove Int
  | StartGame
  | BackToSetup
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
foreign import javascript unsafe "globalThis.playMoveSound()"
  js_playMoveSound :: IO ()
#else
js_playMoveSound :: IO ()
js_playMoveSound = pure ()
#endif

main :: IO ()
main = startApp defaultEvents app
  where
    app = Component
      { model            = initModel
      , hydrateModel     = Nothing
      , update           = updateModel
      , view             = viewModel
      , subs             = []
      , styles           = []
      , scripts          = []
      , mountPoint       = Nothing
      , logLevel         = Off
      , mailbox          = const Nothing
      , bindings         = []
      , eventPropagation = False
      , mount            = Nothing
      , unmount          = Nothing
      }

initModel :: Model
initModel = Model
  { mScreen      = SetupScreen
  , mGameMode    = AiMode
  , mGameState   = initialState Tablut
  , mSelected    = Nothing
  , mValidMoves  = []
  , mVariant     = Tablut
  , mAiSide      = AttackerSide
  , mAiThinking  = False
  , mAiDepth     = 4
  , mAiNodeLimit = 10000
  , mHistory     = []
  }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: Action -> Effect ROOT () Model Action
updateModel = \case
  NoOp -> pure ()

  SetGameMode mode ->
    modify $ \m -> m { mGameMode = mode }

  SetVariant variant ->
    modify $ \m -> m { mVariant = variant }

  StartGame -> do
    m <- get
    let gs = initialState (mVariant m)
    put $ m
      { mScreen     = GameScreen
      , mGameState  = gs
      , mSelected   = Nothing
      , mValidMoves = []
      , mAiThinking = False
      , mHistory    = []
      }
    triggerAi

  BackToSetup ->
    modify $ \m -> m { mScreen = SetupScreen, mAiThinking = False }

  NewGame variant -> do
    m <- get
    put $ m
      { mGameState  = initialState variant
      , mSelected   = Nothing
      , mValidMoves = []
      , mVariant    = variant
      , mAiThinking = False
      , mHistory    = []
      }
    triggerAi

  CellClicked coords -> do
    m <- get
    let gs    = mGameState m
        board = gsBoard gs
        side  = turnSide gs
        piece = pieceAt board coords
        aiBlocked = mGameMode m == AiMode && mAiSide m == side
    if finished (gsResult gs) || mAiThinking m || aiBlocked
      then pure ()
      else case mSelected m of
        Just sel | coords `elem` mValidMoves m -> do
          let gs' = act gs (MoveAction sel coords)
          modify $ const $ m { mGameState = gs', mSelected = Nothing, mValidMoves = []
                             , mHistory = mHistory m ++ [gs] }
          io_ js_playMoveSound
          triggerAi
        _ | canControl side piece -> do
          let moves = getPossibleMovesFrom gs coords
          modify $ const $ m { mSelected = Just coords, mValidMoves = moves }
        _ ->
          modify $ const $ m { mSelected = Nothing, mValidMoves = [] }

  AiMoveComplete move -> do
    m <- get
    if mAiThinking m
      then do
        let gs = mGameState m
            gs' = act gs move
        modify $ const $ m
          { mGameState = gs', mSelected = Nothing
          , mValidMoves = [], mAiThinking = False
          , mHistory = mHistory m ++ [gs] }
        io_ js_playMoveSound
      else pure ()

  SetAiSide side ->
    modify $ \m -> m { mAiSide = side }

  SetAiDepth d ->
    modify $ \m -> m { mAiDepth = max 1 (min 8 d) }

  SetAiNodeLimit n ->
    modify $ \m -> m { mAiNodeLimit = n }

  GotoMove i -> do
    m <- get
    let allStates = mHistory m ++ [mGameState m]
        currentIdx = length allStates - 1
    if i >= 0 && i < currentIdx
      then do
        let target = allStates !! i
            newHistory = take i allStates
        put $ m
          { mGameState  = target
          , mHistory    = newHistory
          , mSelected   = Nothing
          , mValidMoves = []
          , mAiThinking = False
          }
        triggerAi
      else pure ()

-- | Check if the AI should move and trigger the search if so.
triggerAi :: Effect ROOT () Model Action
triggerAi = do
  m <- get
  let gs = mGameState m
  if mGameMode m == AiMode && not (finished (gsResult gs)) && mAiSide m == turnSide gs
    then do
      modify $ \x -> x { mAiThinking = True }
      let cfg = AiConfig (mAiDepth m) (mAiNodeLimit m)
      withSink $ \sink -> do
        threadDelay 100000
        case bestMove cfg gs of
          Nothing   -> sink NoOp
          Just move -> sink (AiMoveComplete move)
    else pure ()

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

viewModel :: () -> Model -> View Model Action
viewModel _ m = case mScreen m of
  SetupScreen -> viewSetup m
  GameScreen  -> viewGame m

-- ---------------------------------------------------------------------------
-- Setup Screen
-- ---------------------------------------------------------------------------

viewSetup :: Model -> View Model Action
viewSetup m =
  H.div_
    [ CSS.style_
      [ CSS.margin "0"
      , CSS.padding "0"
      , CSS.position "fixed"
      , "top" =: "0"
      , "left" =: "0"
      , CSS.width "100%"
      , "height" =: "100%"
      , "overflow-y" =: "auto"
      , "overscroll-behavior" =: "none"
      , CSS.backgroundColor (RGB 30 28 26)
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.justifyContent "flex-start"
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , CSS.boxSizing "border-box"
      , CSS.paddingTop "48px"
      , CSS.paddingBottom "48px"
      ]
    ]
    [ viewTitle
    , setupSection "Mode"
        [ setupBtn (SetGameMode LocalMode) "Local" (mGameMode m == LocalMode)
        , setupBtn (SetGameMode AiMode) "vs AI" (mGameMode m == AiMode)
        , setupBtnDisabled "Multiplayer (coming soon)"
        ]
    , setupSection "Board"
        [ setupBtn (SetVariant Brandubh) "Brandubh 7x7" (mVariant m == Brandubh)
        , setupBtn (SetVariant Tablut) "Tablut 9x9" (mVariant m == Tablut)
        , setupBtn (SetVariant Classic) "Copenhagen 11x11" (mVariant m == Classic)
        , setupBtn (SetVariant Line) "Line 11x11" (mVariant m == Line)
        , setupBtn (SetVariant Tawlbwrdd) "Tawlbwrdd 11x11" (mVariant m == Tawlbwrdd)
        ]
    , if mGameMode m == AiMode then viewSetupAi m else H.div_ [] []
    , H.div_
        [ CSS.style_ [CSS.marginTop "28px"] ]
        [ H.button_
            [ CSS.style_
              [ CSS.backgroundColor (RGB 80 130 80)
              , CSS.color (RGB 240 255 240)
              , CSS.border "2px solid"
              , CSS.borderColor (RGB 120 180 120)
              , CSS.borderRadius "8px"
              , CSS.padding "14px 40px"
              , CSS.cursor "pointer"
              , CSS.fontSize "18px"
              , CSS.fontWeight "bold"
              , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
              , CSS.letterSpacing "2px"
              , "touch-action" =: "manipulation"
              ]
            , SVG.onClick StartGame
            ]
            [ text "START GAME" ]
        ]
    ]

setupSection :: MisoString -> [View Model Action] -> View Model Action
setupSection label children =
  H.div_
    [ CSS.style_
      [ CSS.marginTop "24px"
      , CSS.width "100%"
      , CSS.maxWidth "500px"
      , CSS.textAlign "center"
      ]
    ]
    [ H.div_
        [ CSS.style_
          [ CSS.color (RGB 150 140 125)
          , CSS.fontSize "12px"
          , CSS.letterSpacing "3px"
          , CSS.marginBottom "10px"
          , "text-transform" =: "uppercase"
          ]
        ]
        [ text label ]
    , H.div_
        [ CSS.style_
          [ CSS.display "flex"
          , CSS.gap "8px"
          , CSS.flexWrap "wrap"
          , CSS.justifyContent "center"
          ]
        ]
        children
    ]

setupBtn :: Action -> MisoString -> Bool -> View Model Action
setupBtn action label isActive =
  let bg = if isActive then RGB 80 110 80 else RGB 55 50 45
      fg = if isActive then RGB 220 255 220 else RGB 180 170 155
  in H.button_
    [ CSS.style_
      [ CSS.backgroundColor bg
      , CSS.color fg
      , CSS.border "1px solid"
      , CSS.borderColor (if isActive then RGB 120 160 120 else RGB 70 65 58)
      , CSS.borderRadius "6px"
      , CSS.padding "10px 16px"
      , CSS.cursor "pointer"
      , CSS.fontSize "14px"
      , CSS.fontWeight (if isActive then "bold" else "normal")
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , "touch-action" =: "manipulation"
      ]
    , SVG.onClick action
    ]
    [ text label ]

setupBtnDisabled :: MisoString -> View Model Action
setupBtnDisabled label =
  H.button_
    [ CSS.style_
      [ CSS.backgroundColor (RGB 45 42 38)
      , CSS.color (RGB 100 95 85)
      , CSS.border "1px solid"
      , CSS.borderColor (RGB 55 52 48)
      , CSS.borderRadius "6px"
      , CSS.padding "10px 16px"
      , CSS.cursor "not-allowed"
      , CSS.fontSize "14px"
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , CSS.opacity "0.6"
      , "touch-action" =: "manipulation"
      ]
    , HP.disabled_
    ]
    [ text label ]

viewSetupAi :: Model -> View Model Action
viewSetupAi m =
  H.div_
    [ CSS.style_
      [ CSS.marginTop "20px"
      , CSS.width "100%"
      , CSS.maxWidth "500px"
      , CSS.textAlign "center"
      ]
    ]
    [ H.div_
        [ CSS.style_
          [ CSS.color (RGB 150 140 125)
          , CSS.fontSize "12px"
          , CSS.letterSpacing "3px"
          , CSS.marginBottom "10px"
          , "text-transform" =: "uppercase"
          ]
        ]
        [ text "AI SETTINGS" ]
    -- AI side
    , H.div_
        [ CSS.style_
          [ CSS.display "flex", CSS.gap "8px", CSS.flexWrap "wrap"
          , CSS.justifyContent "center", CSS.marginBottom "8px"
          ]
        ]
        [ setupBtn (SetAiSide AttackerSide) "AI plays Attackers" (mAiSide m == AttackerSide)
        , setupBtn (SetAiSide DefenderSide) "AI plays Defenders" (mAiSide m == DefenderSide)
        ]
    -- Depth
    , H.div_
        [ CSS.style_
          [ CSS.display "flex", CSS.gap "4px", CSS.flexWrap "wrap"
          , CSS.justifyContent "center", CSS.alignItems "center"
          , CSS.marginBottom "8px"
          ]
        ]
        ( H.span_ [CSS.style_ [CSS.color (RGB 180 170 155), CSS.fontSize "13px"]] [text "Depth:"]
        : [ setupBtn (SetAiDepth d) (ms (show d)) (mAiDepth m == d)
          | d <- [1..8]
          ]
        )
    -- Node limit
    , H.div_
        [ CSS.style_
          [ CSS.display "flex", CSS.gap "4px", CSS.flexWrap "wrap"
          , CSS.justifyContent "center", CSS.alignItems "center"
          ]
        ]
        ( H.span_ [CSS.style_ [CSS.color (RGB 180 170 155), CSS.fontSize "13px"]] [text "Nodes:"]
        : [ setupBtn (SetAiNodeLimit n) label (mAiNodeLimit m == n)
          | (n, label) <- [ (1000, "1K"), (5000, "5K")
                          , (10000, "10K"), (50000, "50K"), (100000, "100K")
                          , (0, "None")
                          ]
          ]
        )
    ]

-- ---------------------------------------------------------------------------
-- Game Screen
-- ---------------------------------------------------------------------------

viewGame :: Model -> View Model Action
viewGame m =
  H.div_
    [ CSS.style_
      [ CSS.margin "0"
      , CSS.padding "0"
      , CSS.position "fixed"
      , "top" =: "0"
      , "left" =: "0"
      , CSS.width "100%"
      , "height" =: "100%"
      , "overflow-y" =: "auto"
      , "overscroll-behavior" =: "none"
      , CSS.backgroundColor (RGB 30 28 26)
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.justifyContent "flex-start"
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , CSS.boxSizing "border-box"
      , CSS.paddingTop "16px"
      , CSS.paddingBottom "16px"
      ]
    ]
    [ viewTitle
    , viewBoardPanel m
    , viewStatus m
    , viewMoveHistory m
    , viewGameControls m
    ]

viewTitle :: View Model Action
viewTitle =
  H.div_
    [ CSS.style_
      [ CSS.color (RGB 200 185 160)
      , CSS.fontSize "clamp(20px, 5vw, 32px)"
      , CSS.fontWeight "bold"
      , CSS.letterSpacing "6px"
      , CSS.marginBottom "16px"
      , CSS.textAlign "center"
      , CSS.textShadow "0 2px 8px rgba(0,0,0,0.6)"
      ]
    ]
    [ text "TAFLHOUSE" ]

-- ---------------------------------------------------------------------------
-- Board
-- ---------------------------------------------------------------------------

sqSize :: Int
sqSize = 54

viewBoardPanel :: Model -> View Model Action
viewBoardPanel m =
  let n = boardSize (gsBoard (mGameState m))
      totalPx = sqSize * n
  in H.div_
    [ CSS.style_
      [ CSS.position "relative"
      , CSS.boxShadow "0 8px 32px rgba(0,0,0,0.7)"
      , CSS.borderRadius "4px"
      , CSS.overflow "hidden"
      , CSS.border "3px solid"
      , CSS.borderColor (RGB 50 45 38)
      , CSS.width "100%"
      , CSS.maxWidth (ms totalPx <> "px")
      ]
    ]
    [ viewSVGBoard m ]

viewSVGBoard :: Model -> View Model Action
viewSVGBoard m =
  let gs    = mGameState m
      board = gsBoard gs
      n     = boardSize board
      total = sqSize * n
  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , CSS.style_ [CSS.display "block", "aspect-ratio" =: "1"]
    ]
    ( svgDefs
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    ++ renderSpecialSquares gs n
    ++ renderHighlights m n
    ++ renderValidDots m n
    ++ [ renderPiece n r c (pieceAt board (Coords r c))
       | r <- [0..n-1], c <- [0..n-1]
       , pieceAt board (Coords r c) /= Empty
       ]
    ++ renderLastMove m n
    ++ [ renderClickTarget m n r c | r <- [0..n-1], c <- [0..n-1] ]
    )

svgDefs :: View Model Action
svgDefs =
  SVG.defs_ []
    [ SVG.filter_
        [ HP.id_ "pieceShadow"
        , SP.x_ "-20%", SP.y_ "-20%"
        , HP.width_ "140%", HP.height_ "160%"
        ]
        [ SVG.feDropShadow_
            [ SP.dx_ "0.3", SP.dy_ "0.7"
            , SP.stdDeviation_ "0.5"
            , SP.floodColor_ "#000000"
            , SP.floodOpacity_ "0.45"
            ]
        ]
    ]

-- Square background colors
renderSquareBg :: Int -> Int -> Int -> View Model Action
renderSquareBg _n r c =
  SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ (if even (r + c) then "#d4a76a" else "#8b5e3c")
    ]

-- Mark corners and center (throne)
renderSpecialSquares :: GameState -> Int -> [View Model Action]
renderSpecialSquares gs n =
  let center = n `div` 2
      w      = cornerBaseWidth (gsRules gs)
      corners = [ (r, c)
                | r <- concatMap (\ww -> [ww, n - 1 - ww]) [0..w-1]
                , c <- concatMap (\ww -> [ww, n - 1 - ww]) [0..w-1]
                ]
      markSquare (r, c) color =
        SVG.rect_
          [ SP.x_ (ms (c * sqSize + 2))
          , SP.y_ (ms (r * sqSize + 2))
          , HP.width_ (ms (sqSize - 4))
          , HP.height_ (ms (sqSize - 4))
          , SP.fill_ "none"
          , SP.stroke_ color
          , SP.strokeWidth_ "2"
          , SP.rx_ "3"
          ]
  in map (\pos -> markSquare pos "rgba(180,60,60,0.6)") corners
     ++ [markSquare (center, center) "rgba(80,80,180,0.5)"]

-- Highlight selected square
renderHighlights :: Model -> Int -> [View Model Action]
renderHighlights m _n = case mSelected m of
  Nothing -> []
  Just (Coords r c) ->
    [ SVG.rect_
        [ SP.x_ (ms (c * sqSize))
        , SP.y_ (ms (r * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(80,200,120,0.45)"
        ]
    ]

-- Valid move dots
renderValidDots :: Model -> Int -> [View Model Action]
renderValidDots m _n =
  [ SVG.circle_
      [ SP.cx_ (ms (col coord * sqSize + sqSize `div` 2))
      , SP.cy_ (ms (row coord * sqSize + sqSize `div` 2))
      , SP.r_ (ms (sqSize `div` 5))
      , SP.fill_ "rgba(80,200,120,0.6)"
      ]
  | coord <- mValidMoves m
  ]

-- Last move indicators
renderLastMove :: Model -> Int -> [View Model Action]
renderLastMove m _n = case gsLastAction (mGameState m) of
  Nothing -> []
  Just (MoveAction f t) ->
    [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ "rgba(200,200,80,0.3)"
        ]
    | sq <- [f, t]
    ]

-- Piece rendering
renderPiece :: Int -> Int -> Int -> Piece -> View Model Action
renderPiece _n r c piece =
  let cx = c * sqSize + sqSize `div` 2
      cy = r * sqSize + sqSize `div` 2
      radius = sqSize `div` 2 - 4
      (fill, stroke, label) = case piece of
        Attacker -> ("#2a2a2a", "#111", "A" :: MisoString)
        Defender -> ("#e8e0d0", "#555", "D")
        King     -> ("#ffd700", "#8b6914", "K")
        Empty    -> ("#000", "#000", "")
  in SVG.g_
    [ SP.filter_ "url(#pieceShadow)" ]
    [ SVG.circle_
        [ SP.cx_ (ms cx)
        , SP.cy_ (ms cy)
        , SP.r_ (ms radius)
        , SP.fill_ fill
        , SP.stroke_ stroke
        , SP.strokeWidth_ "2"
        ]
    , SVG.text_
        [ SP.x_ (ms cx)
        , SP.y_ (ms (cy + 1))
        , SP.textAnchor_ "middle"
        , SP.dominantBaseline_ "central"
        , SP.fontSize_ (ms (sqSize `div` 3))
        , SP.fontWeight_ "bold"
        , SP.fill_ (if piece == Attacker then "#ccc" else "#333")
        , SP.fontFamily_ "Arial, sans-serif"
        ]
        [ text label ]
    ]

-- Transparent click targets
renderClickTarget :: Model -> Int -> Int -> Int -> View Model Action
renderClickTarget m _n r c =
  let gs = mGameState m
      side = turnSide gs
      aiBlocked = mGameMode m == AiMode && mAiSide m == side
      blocked = mAiThinking m || aiBlocked || finished (gsResult gs)
      cur = if blocked then "not-allowed" else "pointer"
  in SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ "transparent"
    , CSS.style_ [CSS.cursor cur, "touch-action" =: "manipulation"]
    , SVG.onClick (CellClicked (Coords r c))
    ]

-- ---------------------------------------------------------------------------
-- Status & Controls
-- ---------------------------------------------------------------------------

viewStatus :: Model -> View Model Action
viewStatus m =
  let gs     = mGameState m
      result = gsResult gs
      side   = turnSide gs
      caps   = gsCaptures gs
      isAi   = mGameMode m == AiMode
      (bgColor, fgColor, msg)
        | finished result = case winner result of
            Just AttackerSide -> (RGB 120 30 30, RGB 255 200 200, "Attackers win! " <> desc result)
            Just DefenderSide -> (RGB 30 100 50, RGB 200 255 200, "Defenders win! " <> desc result)
            Nothing           -> (RGB 80 70 40, RGB 240 230 180, "Draw! " <> desc result)
        | mAiThinking m = (RGB 60 55 50, RGB 200 180 160, "AI thinking...")
        | isAi && mAiSide m == side = (RGB 50 45 40, RGB 220 200 180,
            (if side == AttackerSide then "Attacker's turn" else "Defender's turn") <> " (AI)")
        | isAi = (RGB 50 45 40, RGB 220 200 180, "Your turn")
        | side == AttackerSide = (RGB 50 45 40, RGB 220 200 180, "Attacker's turn")
        | otherwise            = (RGB 50 45 40, RGB 220 200 180, "Defender's turn")
  in H.div_
    [ CSS.style_
      [ CSS.backgroundColor bgColor
      , CSS.color fgColor
      , CSS.borderRadius "8px"
      , CSS.padding "12px 20px"
      , CSS.fontSize "16px"
      , CSS.fontWeight "bold"
      , CSS.textAlign "center"
      , CSS.marginTop "16px"
      , CSS.width "100%"
      , CSS.maxWidth (ms (sqSize * 11) <> "px")
      , CSS.boxSizing "border-box"
      , CSS.boxShadow "0 2px 8px rgba(0,0,0,0.4)"
      ]
    ]
    [ text (ms msg)
    , if not (null caps) && not (finished result)
        then H.div_
          [ CSS.style_
            [ CSS.marginTop "4px"
            , CSS.fontSize "12px"
            , CSS.opacity "0.8"
            , CSS.fontWeight "normal"
            ]
          ]
          [ text (ms ("Last move captured " ++ show (length caps) ++ " piece(s)")) ]
        else H.div_ [] []
    ]

viewMoveHistory :: Model -> View Model Action
viewMoveHistory m
  | null (mHistory m) = H.div_ [] []
  | otherwise =
      let allStates = mHistory m ++ [mGameState m]
          n = boardSize (gsBoard (mGameState m))
          currentIdx = length allStates - 1
      in H.div_
        [ CSS.style_
          [ CSS.display "flex"
          , CSS.flexDirection "column"
          , CSS.gap "4px"
          , CSS.marginTop "12px"
          , CSS.alignItems "center"
          , CSS.width "100%"
          , CSS.maxWidth (ms (sqSize * 11) <> "px")
          ]
        ]
        [ H.div_
            [ CSS.style_
              [ CSS.display "flex"
              , CSS.gap "3px"
              , CSS.flexWrap "wrap"
              , CSS.justifyContent "center"
              , CSS.maxHeight "72px"
              , "overflow-y" =: "auto"
              , CSS.padding "4px"
              ]
            ]
            [ moveBtn i gs n (i == currentIdx)
            | (i, gs) <- zip [0..] allStates
            ]
        ]

moveBtn :: Int -> GameState -> Int -> Bool -> View Model Action
moveBtn idx gs n isCurrent =
  let label = case gsLastAction gs of
        Nothing -> "Start"
        Just (MoveAction f t) ->
          let side = opponentSide gs
              sideChar = case side of
                AttackerSide -> "A"
                DefenderSide -> "D"
          in ms (show idx) <> "." <> sideChar <> " "
               <> ms (coordStr n f) <> "-" <> ms (coordStr n t)
      bg = if isCurrent then RGB 80 100 120 else RGB 50 48 45
      fg = if isCurrent then RGB 200 220 255 else RGB 150 145 135
  in H.button_
    [ CSS.style_
      [ CSS.backgroundColor bg
      , CSS.color fg
      , CSS.border "1px solid"
      , CSS.borderColor (if isCurrent then RGB 100 130 170 else RGB 65 60 55)
      , CSS.borderRadius "4px"
      , CSS.padding "2px 5px"
      , CSS.cursor (if isCurrent then "default" else "pointer")
      , CSS.fontSize "11px"
      , CSS.fontWeight (if isCurrent then "bold" else "normal")
      , CSS.fontFamily "'Consolas', 'Courier New', monospace"
      , "touch-action" =: "manipulation"
      ]
    , SVG.onClick (GotoMove idx)
    ]
    [ text label ]

coordStr :: Int -> Coords -> String
coordStr n (Coords r c) = [toEnum (fromEnum 'a' + c)] ++ show (n - r)

viewGameControls :: Model -> View Model Action
viewGameControls m =
  H.div_
    [ CSS.style_
      [ CSS.display "flex"
      , CSS.gap "8px"
      , CSS.marginTop "12px"
      , CSS.flexWrap "wrap"
      , CSS.justifyContent "center"
      ]
    ]
    [ ctrlBtn BackToSetup "Back to Setup" False
    , variantBtn m Brandubh "New: Brandubh 7x7"
    , variantBtn m Tablut "New: Tablut 9x9"
    , variantBtn m Classic "New: Copenhagen 11x11"
    , variantBtn m Line "New: Line 11x11"
    , variantBtn m Tawlbwrdd "New: Tawlbwrdd 11x11"
    ]

ctrlBtn :: Action -> MisoString -> Bool -> View Model Action
ctrlBtn action label isActive =
  let bg = if isActive then RGB 80 110 80 else RGB 55 50 45
      fg = if isActive then RGB 220 255 220 else RGB 180 170 155
  in H.button_
    [ CSS.style_
      [ CSS.backgroundColor bg
      , CSS.color fg
      , CSS.border "1px solid"
      , CSS.borderColor (if isActive then RGB 120 160 120 else RGB 70 65 58)
      , CSS.borderRadius "6px"
      , CSS.padding "6px 10px"
      , CSS.cursor "pointer"
      , CSS.fontSize "12px"
      , CSS.fontWeight (if isActive then "bold" else "normal")
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , "touch-action" =: "manipulation"
      ]
    , SVG.onClick action
    ]
    [ text label ]

variantBtn :: Model -> BoardVariant -> MisoString -> View Model Action
variantBtn m variant label =
  let isActive = mVariant m == variant
      bg = if isActive then RGB 80 110 80 else RGB 55 50 45
      fg = if isActive then RGB 220 255 220 else RGB 180 170 155
  in H.button_
    [ CSS.style_
      [ CSS.backgroundColor bg
      , CSS.color fg
      , CSS.border "1px solid"
      , CSS.borderColor (if isActive then RGB 120 160 120 else RGB 70 65 58)
      , CSS.borderRadius "6px"
      , CSS.padding "8px 14px"
      , CSS.cursor "pointer"
      , CSS.fontSize "13px"
      , CSS.fontWeight (if isActive then "bold" else "normal")
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , "touch-action" =: "manipulation"
      ]
    , SVG.onClick (NewGame variant)
    ]
    [ text label ]
