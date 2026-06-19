{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Miso hiding ((!!))
import qualified Miso.CSS as CSS
import Miso.CSS.Color
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Tafl.Types
import Tafl.Rules (copenhagen, BoardVariant(..), variantDefaultRules)
import Tafl.Board (variantBoard)
import Tafl.Move  (getPossibleMovesFrom, getPossibleActions, isActionPossible)
import Tafl.Game  (act, isGameOver, initialState)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

data Model = Model
  { mGameState  :: !GameState
  , mSelected   :: Maybe Coords
  , mValidMoves :: [Coords]
  , mVariant    :: !BoardVariant
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Action
-- ---------------------------------------------------------------------------

data Action
  = CellClicked Coords
  | NewGame BoardVariant
  | NoOp
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
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
  { mGameState  = initialState Classic
  , mSelected   = Nothing
  , mValidMoves = []
  , mVariant    = Classic
  }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

updateModel :: Action -> Effect ROOT () Model Action
updateModel = \case
  NoOp -> pure ()

  NewGame variant -> modify $ \_ -> Model
    { mGameState  = initialState variant
    , mSelected   = Nothing
    , mValidMoves = []
    , mVariant    = variant
    }

  CellClicked coords -> modify $ \m ->
    let gs    = mGameState m
        board = gsBoard gs
        side  = turnSide gs
        piece = pieceAt board coords
    in
    -- Game over? Ignore clicks
    if finished (gsResult gs) then m
    -- Clicking a selected piece's valid move destination -> make the move
    else case mSelected m of
      Just sel | coords `elem` mValidMoves m ->
        let gs' = act gs (MoveAction sel coords)
        in m { mGameState = gs', mSelected = Nothing, mValidMoves = [] }
      -- Clicking own piece -> select it
      _ | canControl side piece ->
        let moves = getPossibleMovesFrom gs coords
        in m { mSelected = Just coords, mValidMoves = moves }
      -- Clicking elsewhere -> deselect
      _ -> m { mSelected = Nothing, mValidMoves = [] }

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
    ++ [ renderClickTarget n r c | r <- [0..n-1], c <- [0..n-1] ]
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
renderClickTarget :: Int -> Int -> Int -> View Model Action
renderClickTarget _n r c =
  SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ "transparent"
    , CSS.style_ [CSS.cursor "pointer", "touch-action" =: "manipulation"]
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
      (bgColor, fgColor, msg)
        | finished result = case winner result of
            Just AttackerSide -> (RGB 120 30 30, RGB 255 200 200, "Attackers win! " <> desc result)
            Just DefenderSide -> (RGB 30 100 50, RGB 200 255 200, "Defenders win! " <> desc result)
            Nothing           -> (RGB 80 70 40, RGB 240 230 180, "Draw! " <> desc result)
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
