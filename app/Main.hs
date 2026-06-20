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

data Model = Model
  { mGameState   :: !GameState
  , mSelected    :: Maybe Coords
  , mValidMoves  :: [Coords]
  , mVariant     :: !BoardVariant
  , mAiEnabled   :: !Bool
  , mAiSide      :: !Side
  , mAiThinking  :: !Bool
  , mAiDepth     :: !Int
  , mAiNodeLimit :: !Int
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Action
-- ---------------------------------------------------------------------------

data Action
  = CellClicked Coords
  | NewGame BoardVariant
  | NoOp
  | AiMoveComplete MoveAction
  | ToggleAi
  | SetAiSide Side
  | SetAiDepth Int
  | SetAiNodeLimit Int
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
      , subs             = [\sink -> threadDelay 100000 >> sink (NewGame Tablut)]
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
  { mGameState   = initialState Tablut
  , mSelected    = Nothing
  , mValidMoves  = []
  , mVariant     = Tablut
  , mAiEnabled   = True
  , mAiSide      = AttackerSide
  , mAiThinking  = False
  , mAiDepth     = 4
  , mAiNodeLimit = 0
  }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: Action -> Effect ROOT () Model Action
updateModel = \case
  NoOp -> pure ()

  NewGame variant -> do
    m <- get
    put $ m
      { mGameState  = initialState variant
      , mSelected   = Nothing
      , mValidMoves = []
      , mVariant    = variant
      , mAiThinking = False
      }
    triggerAi

  CellClicked coords -> do
    m <- get
    let gs    = mGameState m
        board = gsBoard gs
        side  = turnSide gs
        piece = pieceAt board coords
    if finished (gsResult gs) || mAiThinking m || (mAiEnabled m && mAiSide m == side)
      then pure ()
      else case mSelected m of
        Just sel | coords `elem` mValidMoves m -> do
          let gs' = act gs (MoveAction sel coords)
          modify $ const $ m { mGameState = gs', mSelected = Nothing, mValidMoves = [] }
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
        let gs' = act (mGameState m) move
        modify $ const $ m
          { mGameState = gs', mSelected = Nothing
          , mValidMoves = [], mAiThinking = False }
        io_ js_playMoveSound
      else pure ()

  ToggleAi -> do
    modify $ \m -> m { mAiEnabled = not (mAiEnabled m), mAiThinking = False }
    triggerAi

  SetAiSide side -> do
    modify $ \m -> m { mAiSide = side, mAiThinking = False }
    triggerAi

  SetAiDepth d ->
    modify $ \m -> m { mAiDepth = max 1 (min 8 d) }

  SetAiNodeLimit n ->
    modify $ \m -> m { mAiNodeLimit = n }

-- | Check if the AI should move and trigger the search if so.
triggerAi :: Effect ROOT () Model Action
triggerAi = do
  m <- get
  let gs = mGameState m
  if mAiEnabled m && not (finished (gsResult gs)) && mAiSide m == turnSide gs
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
viewModel _ m =
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
    , viewAiControls m
    , viewControls m
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
      blocked = mAiThinking m
             || (mAiEnabled m && mAiSide m == side)
             || finished (gsResult gs)
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
      isAiTurn = mAiEnabled m && mAiSide m == side
      (bgColor, fgColor, msg)
        | finished result = case winner result of
            Just AttackerSide -> (RGB 120 30 30, RGB 255 200 200, "Attackers win! " <> desc result)
            Just DefenderSide -> (RGB 30 100 50, RGB 200 255 200, "Defenders win! " <> desc result)
            Nothing           -> (RGB 80 70 40, RGB 240 230 180, "Draw! " <> desc result)
        | mAiThinking m = (RGB 60 55 50, RGB 200 180 160, "AI thinking...")
        | side == AttackerSide = (RGB 50 45 40, RGB 220 200 180,
            "Attacker's turn" <> if isAiTurn then " (AI)" else "")
        | otherwise            = (RGB 50 45 40, RGB 220 200 180,
            "Defender's turn" <> if isAiTurn then " (AI)" else "")
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

viewAiControls :: Model -> View Model Action
viewAiControls m =
  H.div_
    [ CSS.style_
      [ CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.gap "8px"
      , CSS.marginTop "12px"
      , CSS.alignItems "center"
      , CSS.width "100%"
      , CSS.maxWidth (ms (sqSize * 11) <> "px")
      ]
    ]
    [ -- Row 1: Toggle + Side
      H.div_
        [ CSS.style_ [CSS.display "flex", CSS.gap "8px", CSS.flexWrap "wrap", CSS.justifyContent "center"] ]
        [ ctrlBtn ToggleAi
            (if mAiEnabled m then "AI: ON" else "AI: OFF")
            (mAiEnabled m)
        , ctrlBtn (SetAiSide AttackerSide) "AI: Attackers"
            (mAiEnabled m && mAiSide m == AttackerSide)
        , ctrlBtn (SetAiSide DefenderSide) "AI: Defenders"
            (mAiEnabled m && mAiSide m == DefenderSide)
        ]
    , -- Row 2: Depth
      H.div_
        [ CSS.style_
          [ CSS.display "flex", CSS.gap "4px", CSS.flexWrap "wrap"
          , CSS.justifyContent "center", CSS.alignItems "center"
          ]
        ]
        ( H.span_ [CSS.style_ [CSS.color (RGB 180 170 155), CSS.fontSize "12px"]] [text "Depth:"]
        : [ ctrlBtn (SetAiDepth d) (ms (show d)) (mAiDepth m == d)
          | d <- [1..8]
          ]
        )
    , -- Row 3: Node limit
      H.div_
        [ CSS.style_
          [ CSS.display "flex", CSS.gap "4px", CSS.flexWrap "wrap"
          , CSS.justifyContent "center", CSS.alignItems "center"
          ]
        ]
        ( H.span_ [CSS.style_ [CSS.color (RGB 180 170 155), CSS.fontSize "12px"]] [text "Nodes:"]
        : [ ctrlBtn (SetAiNodeLimit n) label (mAiNodeLimit m == n)
          | (n, label) <- [ (0, "None"), (1000, "1K"), (5000, "5K")
                          , (10000, "10K"), (50000, "50K"), (100000, "100K")
                          ]
          ]
        )
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

viewControls :: Model -> View Model Action
viewControls m =
  H.div_
    [ CSS.style_
      [ CSS.display "flex"
      , CSS.gap "8px"
      , CSS.marginTop "12px"
      , CSS.flexWrap "wrap"
      , CSS.justifyContent "center"
      ]
    ]
    [ variantBtn m Brandubh "Brandubh 7x7"
    , variantBtn m Tablut "Tablut 9x9"
    , variantBtn m Classic "Copenhagen 11x11"
    , variantBtn m Line "Line 11x11"
    , variantBtn m Tawlbwrdd "Tawlbwrdd 11x11"
    ]

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
