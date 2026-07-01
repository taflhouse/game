{-# LANGUAGE OverloadedStrings #-}
module App.Board
  ( -- * Constants
    sqSize
    -- * Pure helpers
  , coordStr
  , formatClockMs
  , showScore
  , showPct
  , pieceKey
    -- * SVG primitives
  , svgDefs
  , renderSquareBg
  , renderSpecialSquares
  , renderPiece
  , renderLastMove
    -- * Composite views
  , viewBasicSVGBoard
  , viewBoardContainer
  , viewEvalBar
  , viewClock
  ) where

import Miso.String (MisoString, ms)
import Miso hiding ((!!))
import Miso.CSS (style_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Tafl.Board
import Tafl.Rules (RuleSet(..))
import Tafl.Game.State

import App.FFI (js_formatDeadline)
import App.Model (TimeControl(..))

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

sqSize :: Int
sqSize = 54

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

-- | Convert board coordinates to algebraic notation (e.g., "a9").
coordStr :: Int -> Coords -> String
coordStr n (Coords r c) = [toEnum (fromEnum 'a' + c)] ++ show (n - r)

-- | Format milliseconds as MM:SS or M:SS.t (under 1 minute).
formatClockMs :: Int -> MisoString
formatClockMs millis
  | millis <= 0   = "0:00"
  | millis < 60000 =
      let totalSecs = millis `div` 1000
          tenths    = (millis `mod` 1000) `div` 100
      in ms (show totalSecs) <> "." <> ms (show tenths)
  | otherwise =
      let totalSecs = millis `div` 1000
          mins      = totalSecs `div` 60
          secs      = totalSecs `mod` 60
      in ms (show mins) <> ":" <> ms (if secs < 10 then "0" ++ show secs else show secs)

showScore :: Int -> String
showScore s =
  let (q, r) = abs s `divMod` 100
      sign = if s < 0 then "-" else ""
  in sign ++ show q ++ "." ++ (if r < 10 then "0" else "") ++ show r

showPct :: Double -> String
showPct d =
  let n = round d :: Int
  in show n

-- | Compute a stable key for a piece at (r, c).
-- The piece that just moved gets a key based on its FROM position so the DOM
-- element identity is preserved from the previous render, causing the CSS
-- transition to animate the slide.  All other pieces use position-based keys.
pieceKey :: Maybe MoveAction -> Int -> Int -> MisoString
pieceKey mLastAction r c = case mLastAction of
  Just (MoveAction from to)
    | to == Coords r c -> "p-" <> ms (row from) <> "-" <> ms (col from)
  _ -> "p-" <> ms r <> "-" <> ms c

-- ---------------------------------------------------------------------------
-- SVG primitives
-- ---------------------------------------------------------------------------

svgDefs :: View model action
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

-- | Square background colors (themed via CSS variables).
renderSquareBg :: Int -> Int -> Int -> View model action
renderSquareBg _n r c =
  SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , style_ [("fill", if even (r + c) then "var(--muted)" else "var(--accent)")]
    ]

-- | Mark corners and center (throne).
renderSpecialSquares :: GameState -> Int -> [View model action]
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
  in map (\pos -> markSquare pos "var(--piece-king)") corners
     ++ [markSquare (center, center) "var(--piece-defender)"]

-- | Piece rendering with CSS transition support.
-- Uses transform:translate() so pieces slide smoothly when the DOM element
-- is reused via key-based reconciliation.
renderPiece :: MisoString -> Bool -> Int -> Int -> Int -> Piece -> View model action
renderPiece k animate _n r c piece =
  let tx = c * sqSize
      ty = r * sqSize
      half = sqSize `div` 2
      radius = half - 4
      translateVal = "translate(" <> ms tx <> "px," <> ms ty <> "px)"
      styles = ("transform", translateVal)
             : [("transition", "transform 150ms ease-out") | animate]
      (fill, stroke, label) = case piece of
        Attacker -> ("var(--piece-attacker)", "var(--border)", "A" :: MisoString)
        Defender -> ("var(--piece-defender)", "var(--border)", "D")
        King     -> ("var(--piece-king)", "var(--piece-king-stroke)", "K")
        Empty    -> ("#000", "#000", "")
  in SVG.g_
    [ key_ k
    , style_ styles
    , SP.filter_ "url(#pieceShadow)"
    ]
    [ SVG.circle_
        [ SP.cx_ (ms half)
        , SP.cy_ (ms half)
        , SP.r_ (ms radius)
        , SP.fill_ fill
        , SP.stroke_ stroke
        , SP.strokeWidth_ "2"
        ]
    , SVG.text_
        [ SP.x_ (ms half)
        , SP.y_ (ms (half + 1))
        , SP.textAnchor_ "middle"
        , SP.dominantBaseline_ "central"
        , SP.fontSize_ (ms (sqSize `div` 3))
        , SP.fontWeight_ "bold"
        , SP.fill_ (case piece of
            Attacker -> "var(--piece-attacker-fg)"
            King     -> "var(--piece-king-fg)"
            Defender -> "var(--piece-defender-fg)"
            _        -> "#333")
        , SP.fontFamily_ "Arial, sans-serif"
        ]
        [ text label ]
    ]

-- | Last move indicators (colored by the side that moved).
renderLastMove :: GameState -> Int -> [View model action]
renderLastMove gs _n = case gsLastAction gs of
  Nothing -> []
  Just (MoveAction f t) ->
    let movedPiece = pieceAt (gsBoard gs) t
        hlColor = case movedPiece of
          King     -> "color-mix(in oklch, var(--piece-king) 40%, transparent)"
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 30%, transparent)"
          _        -> "color-mix(in oklch, var(--piece-defender) 50%, transparent)"
    in [ SVG.rect_
        [ SP.x_ (ms (col sq * sqSize))
        , SP.y_ (ms (row sq * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ hlColor
        ]
    | sq <- [f, t]
    ]

-- ---------------------------------------------------------------------------
-- Composite views
-- ---------------------------------------------------------------------------

-- | Render a basic SVG board: squares, special squares, pieces, last move,
--   plus any extra overlays (highlights, valid dots, click targets, etc.).
viewBasicSVGBoard :: GameState -> [View model action] -> View model action
viewBasicSVGBoard gs extras =
  let board = gsBoard gs
      n     = boardSize board
      total = sqSize * n
  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , HP.class_ "block aspect-square"
    ]
    ( svgDefs
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    ++ renderSpecialSquares gs n
    ++ [ SVG.g_ []
         [ renderPiece (pieceKey (gsLastAction gs) r c) True n r c (pieceAt board (Coords r c))
         | r <- [0..n-1], c <- [0..n-1]
         , pieceAt board (Coords r c) /= Empty
         ]
       ]
    ++ renderLastMove gs n
    ++ extras
    )

-- | Board container with sizing (fullscreen-aware).
viewBoardContainer
  :: Bool          -- ^ fullscreen?
  -> Bool          -- ^ zen mode?
  -> Int           -- ^ board dimension (n)
  -> View model action  -- ^ board content (SVG)
  -> View model action
viewBoardContainer fs zen n content =
  let totalPx = sqSize * n
      fsSize = if zen
        then "85vmin"
        else "clamp(50vmin, calc(100vh - 29rem), 85vmin)"
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border"
    , style_ (if fs
        then [("width", fsSize), ("height", fsSize)]
        else [("width", ms totalPx <> "px"), ("max-width", "calc(100vw - 3rem)")])
    ]
    [ content ]

-- | Evaluation bar. Positive = attackers favored, negative = defenders.
viewEvalBar :: Int -> View model action
viewEvalBar score =
  let clamped = max (-1500) (min 1500 score)
      attackerPct = 50.0 + fromIntegral clamped / 1500.0 * 50.0 :: Double
      defenderPct = 100.0 - attackerPct :: Double
      displayScore = if score >= 0
        then "+" <> ms (showScore score)
        else ms (showScore score)
  in H.div_
    [ HP.class_ "flex flex-col rounded overflow-hidden border border-border"
    , style_ [ ("width", "20px"), ("flex-shrink", "0"), ("position", "relative") ]
    ]
    [ H.div_
        [ HP.class_ "w-full transition-all duration-300"
        , style_ [ ("height", ms (showPct attackerPct) <> "%")
                 , ("background", "var(--piece-attacker)") ]
        ] []
    , H.div_
        [ HP.class_ "w-full transition-all duration-300"
        , style_ [ ("height", ms (showPct defenderPct) <> "%")
                 , ("background", "var(--piece-defender)") ]
        ] []
    , H.div_
        [ HP.class_ "absolute text-center"
        , style_ [ ("font-size", "9px"), ("line-height", "1"), ("width", "20px")
                 , ("top", "50%"), ("transform", "translateY(-50%)")
                 , ("color", "var(--muted-foreground)"), ("pointer-events", "none")
                 , ("mix-blend-mode", "difference"), ("font-weight", "bold") ]
        ]
        [ text displayScore ]
    ]

-- | Render a clock row above or below the board.
viewClock :: Int -> MisoString -> Int -> Bool -> TimeControl -> Maybe MisoString -> Bool -> View model action
viewClock n name timeMs isActive tc mDeadline isTop =
  let lowTime = timeMs < 30000 && timeMs > 0
      activeCls = if isActive then " font-bold" else " text-muted-foreground"
      lowCls = if lowTime && isActive then " text-destructive" else ""
      marginStyle = if isTop then [("margin-bottom", "0.3em"), ("margin-top", "1em")]
                             else [("margin-top", "0.3em")]
      timeDisplay = case tc of
        BlitzControl _ -> formatClockMs timeMs
        DailyControl _ -> case mDeadline of
          Just d | isActive -> js_formatDeadline d
          _                 -> "--:--"
        NoTimeControl -> ""
  in H.div_
    [ HP.class_ ("flex justify-between items-center w-full px-2 text-sm font-mono" <> activeCls <> lowCls)
    , style_ (("max-width", ms (sqSize * n) <> "px") : marginStyle)
    ]
    [ H.span_ [] [ text name ]
    , H.span_ [ HP.class_ "tabular-nums" ] [ text timeDisplay ]
    ]
