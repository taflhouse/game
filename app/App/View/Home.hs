{-# LANGUAGE OverloadedStrings #-}
module App.View.Home (viewHome) where

import Miso
import Miso.CSS (style_)
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import App.JSON (GameRecord(..))
import App.Model
import App.Action
import App.FFI (js_formatDate)

-- ---------------------------------------------------------------------------
-- Home Screen
-- ---------------------------------------------------------------------------

viewHome :: Model -> View Model Action
viewHome m =
  H.div_
    [ HP.class_ "w-full max-w-2xl"
    , style_ [("margin-top", "4em")]
    ]
    ( [ H.div_
          [ HP.class_ "flex flex-col items-center mb-6 w-full max-w-md mx-auto" ]
          [ H.button_
              [ HP.class_ "btn-lg w-full"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GotoConfig
              ]
              [ text "New Game" ]
          , viewOrDivider
          , H.span_
              [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GotoJoin
              ]
              [ text "Join Game" ]
          ]
      ] ++ homeContent
    )
  where
    homeContent
      | not (mSessionChecked m) = []
      | otherwise = case mSession m of
      Just _ | mGamesLoading m ->
        [ H.div_
            [ HP.class_ "text-center text-muted-foreground animate-pulse" ]
            [ text "Loading games..." ]
        ]
      Just _ | not (null (mPastGames m)) ->
        [ H.div_
            [ HP.class_ "flex justify-between items-center mb-4" ]
            [ H.h2_
                [ HP.class_ "text-lg font-semibold text-foreground" ]
                [ text "Your Games" ]
            ]
        , viewPastGamesTable (mPastGames m)
        ]
      Just _ -> []
      Nothing ->
        [ H.div_
            [ HP.class_ "text-center"
            , style_ [("margin-top", "2em"), ("position", "relative")]
            ]
            [ H.p_
              [ HP.class_ "text-muted-foreground text-sm italic"
              , style_ [("text-decoration", "underline dotted"), ("text-underline-offset", "4px"), ("cursor", "pointer")]
              , SVG.onClick ToggleQuoteRef
              ]
              [ text "\"They played tafl in the meadow and were merry,\"" ]
          , if mShowQuoteRef m
              then H.div_ []
                [ H.div_
                    [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "49") ]
                    , SVG.onClick DismissQuoteRef
                    ] []
                , H.div_
                    [ HP.class_ "card p-4 text-left"
                    , style_ [ ("position", "absolute"), ("top", "100%"), ("left", "50%")
                             , ("transform", "translateX(-50%)"), ("margin-top", "0.5em")
                             , ("width", "18rem"), ("z-index", "50")
                             ]
                    ]
                    [ H.p_
                        [ HP.class_ "text-sm text-muted-foreground" ]
                        [ text "Vǫluspá, stanza 8" ]
                    ]
                ]
              else text ""
          ]
        ]

-- ---------------------------------------------------------------------------
-- Past Games Table
-- ---------------------------------------------------------------------------

viewPastGamesTable :: [GameRecord] -> View Model Action
viewPastGamesTable games =
  H.div_
    [ HP.class_ "overflow-x-auto"
    ]
    [ H.table_
        [ HP.class_ "table w-full"
        ]
        [ H.thead_
            []
            [ H.tr_
                []
                [ H.th_ [] [ text "Variant" ]
                , H.th_ [] [ text "Mode" ]
                , H.th_ [] [ text "Result" ]
                , H.th_ [] [ text "Moves" ]
                , H.th_ [] [ text "Date" ]
                ]
            ]
        , H.tbody_
            []
            (map viewGameRow games)
        ]
    ]

viewGameRow :: GameRecord -> View Model Action
viewGameRow gr =
  let winText = case grWinner gr of
        Just "attacker" -> "Attackers won"
        Just "defender" -> "Defenders won"
        _               -> "Draw"
      modeText = case grGameMode gr of
        "ai"          -> "vs AI"
        "local"       -> "Practice"
        "multiplayer" -> "Multiplayer"
        _             -> grGameMode gr
      cells =
        [ H.td_ [] [ text (grVariant gr) ]
        , H.td_ [] [ text modeText ]
        , H.td_ [] [ text winText ]
        , H.td_ [] [ text (ms (show (grTotalMoves gr))) ]
        , H.td_ [ HP.class_ "text-muted-foreground" ] [ text (js_formatDate (grPlayedAt gr)) ]
        ]
  in case grId gr of
    Just gid -> H.tr_
      [ HP.class_ "cursor-pointer hover:bg-muted/50"
      , SVG.onClick (GotoReplay gid)
      ] cells
    Nothing -> H.tr_ [] cells

viewOrDivider :: View Model Action
viewOrDivider =
  H.div_
    [ HP.class_ "flex items-center gap-3 w-full"
    , style_ [("margin-top", "1em"), ("margin-bottom", "1em")]
    ]
    [ H.div_ [ HP.class_ "flex-1 border-t border-border" ] []
    , H.span_ [ HP.class_ "text-xs text-muted-foreground uppercase" ] [ text "or" ]
    , H.div_ [ HP.class_ "flex-1 border-t border-border" ] []
    ]
