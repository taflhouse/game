{-# LANGUAGE OverloadedStrings #-}
module App.Replay.View (viewReplay) where

import Miso hiding ((!!))
import Miso.CSS (style_)
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import Tafl.Board (boardSize, Side(..), Piece(..), MoveAction(..), pieceAt)
import Tafl.Game.State (GameState, gsBoard, gsLastAction, opponentSide)

import App.JSON (GameRecord(..))
import App.Board (sqSize, coordStr, viewBasicSVGBoard, viewBoardContainer, viewEvalBar)
import App.Replay.Model
import App.Replay.Action

-- | Main replay view
viewReplay :: ReplayProps -> ReplayModel -> View ReplayModel ReplayAction
viewReplay props rm
  | rmReplayNotFound rm =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8"
      ]
      [ text "This game is private or doesn't exist." ]
  | Nothing <- rmReplayGame rm =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8 animate-pulse"
      ]
      [ text "Loading game..." ]
  | Just gr <- rmReplayGame rm =
    let zen = rpZenMode props
    in H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ if zen then text "" else viewReplayHeader gr
      , case rmReplayStates rm of
          [] -> H.div_
            [ HP.class_ "card p-6 text-center mt-4"
            ]
            [ text "No move data available for this game." ]
          states ->
            let gs = states !! rmReplayIndex rm
                n = boardSize (gsBoard gs)
            in H.div_
              [ HP.class_ "w-full flex flex-col items-center"
              ]
              [ H.div_
                  [ HP.class_ "flex flex-row items-stretch justify-center gap-2"
                  , style_ [("margin-top", "1em")]
                  ]
                  [ if not zen then viewEvalBar (rmEvalScore rm) else text ""
                  , viewReplayBoardPanel props rm gs
                  ]
              , viewReplayControls rm n
              , if zen then text "" else viewReplayMoveList rm n
              ]
      , viewReplayZenHint props
      ]

viewReplayHeader :: GameRecord -> View ReplayModel ReplayAction
viewReplayHeader gr =
  let winText = case grWinner gr of
        Just "attacker" -> "Attackers won"
        Just "defender" -> "Defenders won"
        _               -> "Draw"
  in H.div_
    [ HP.class_ "text-center mb-2"
    , style_ [("margin-top", "2em")]
    ]
    [ H.h2_
        [ HP.class_ "text-lg font-bold" ]
        [ text (grVariant gr) ]
    , H.p_
        [ HP.class_ "text-sm text-muted-foreground" ]
        [ text (winText <> " · " <> ms (show (grTotalMoves gr)) <> " moves") ]
    ]

viewReplayBoardPanel :: ReplayProps -> ReplayModel -> GameState -> View ReplayModel ReplayAction
viewReplayBoardPanel props _rm gs =
  let n = boardSize (gsBoard gs)
  in viewBoardContainer (rpIsFullscreen props) (rpZenMode props) n (viewBasicSVGBoard gs [])

viewReplayControls :: ReplayModel -> Int -> View ReplayModel ReplayAction
viewReplayControls rm n =
  let idx = rmReplayIndex rm
      maxIdx = length (rmReplayStates rm) - 1
  in H.div_
    [ HP.class_ "flex items-center justify-center gap-2 my-4 w-full"
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ replayBtn (RGotoMove 0) "|<" (idx > 0)
    , replayBtn (RGotoMove (idx - 1)) "<" (idx > 0)
    , H.span_
        [ HP.class_ "text-sm font-mono text-muted-foreground min-w-[5em] text-center" ]
        [ text (ms (show idx) <> " / " <> ms (show maxIdx)) ]
    , replayBtn (RGotoMove (idx + 1)) ">" (idx < maxIdx)
    , replayBtn (RGotoMove maxIdx) ">|" (idx < maxIdx)
    , replayBtn RToggleZen "Zen" True
    , replayBtn RToggleFullscreen "FS" True
    ]

replayBtn :: ReplayAction -> MisoString -> Bool -> View ReplayModel ReplayAction
replayBtn action label enabled =
  H.button_
    [ HP.class_ (if enabled
        then "btn btn-outline btn-sm text-foreground"
        else "btn btn-outline btn-sm text-muted-foreground opacity-50 cursor-not-allowed")
    , style_ [("touch-action", "manipulation"), ("min-width", "2.5em")]
    , SVG.onClick (if enabled then action else RNoOp)
    ]
    [ text label ]

viewReplayMoveList :: ReplayModel -> Int -> View ReplayModel ReplayAction
viewReplayMoveList rm n =
  case grMoves =<< rmReplayGame rm of
    Nothing -> H.div_ [] []
    Just moves | null moves -> H.div_ [] []
    Just moves ->
      let states = rmReplayStates rm
          idx = rmReplayIndex rm
      in H.div_
        [ HP.class_ "flex flex-col gap-1 items-center w-full"
        , style_ [("max-width", ms (sqSize * n) <> "px")]
        ]
        [ H.div_
            [ HP.class_ "flex justify-between items-center w-full"
            , style_ [("margin-bottom", "0.4em")]
            ]
            [ H.span_
                [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase" ]
                [ text "MOVES" ]
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ replayMoveBtn i move n (i == idx) states
            | (i, move) <- reverse (zip [1..] moves)
            ]
        ]

-- | Zen mode hint (fixed overlay at bottom of screen)
viewReplayZenHint :: ReplayProps -> View ReplayModel ReplayAction
viewReplayZenHint props
  | rpZenHint props =
    H.div_
      [ HP.class_ "card px-4 py-2 text-sm text-muted-foreground shadow-lg"
      , style_ [ ("position", "fixed"), ("bottom", "1.5rem"), ("left", "50%")
               , ("transform", "translateX(-50%)"), ("z-index", "9999")
               , ("pointer-events", "none")
               ]
      ]
      [ H.span_ [ HP.class_ "hidden sm:inline" ] [ text "Triple-click board to exit zen mode" ]
      , H.span_ [ HP.class_ "sm:hidden" ] [ text "Triple-tap board to exit zen mode" ]
      ]
  | otherwise = text ""

replayMoveBtn :: Int -> MoveAction -> Int -> Bool -> [GameState] -> View ReplayModel ReplayAction
replayMoveBtn idx (MoveAction _f t) n isCurrent states =
  let gs = if idx < length states then states !! idx else states !! (length states - 1)
      moveSide = opponentSide gs
      movedPiece = pieceAt (gsBoard gs) t
      pointer = if isCurrent then "> " else "  "
      sideChar = case moveSide of
          AttackerSide -> "A"
          DefenderSide -> "D"
      la = case gsLastAction gs of
        Just (MoveAction f' t') -> pointer <> ms (show idx) <> ". " <> sideChar <> " "
              <> ms (coordStr n f') <> "-" <> ms (coordStr n t')
        _ -> pointer <> ms (show idx) <> ". " <> sideChar
      (textColor, borderColor) = case movedPiece of
        Attacker -> ("var(--piece-attacker)", "var(--piece-attacker)")
        King     -> ("var(--piece-king)", "var(--piece-king)")
        _        -> ("var(--piece-defender)", "var(--piece-defender)")
      activeCls = " border-l-2"
      moveStyle = [("color", textColor), ("border-left-color", borderColor)]
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted/50 cursor-pointer bg-transparent border-0 text-foreground" <> activeCls)
    , style_ (("touch-action", "manipulation") : moveStyle)
    , SVG.onClick (RGotoMove idx)
    ]
    [ text la ]
