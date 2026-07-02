{-# LANGUAGE OverloadedStrings #-}
module App.View.Profile (viewProfile, viewProfileEdit) where

import Prelude hiding ((.))
import Control.Category ((.))
import Miso
import Miso.CSS (style_)
import Miso.String (MisoString, ms)
import Miso.Lens ((^.))
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import App.JSON (Profile(..), GameRecord(..))
import App.Model
import App.Action

-- ---------------------------------------------------------------------------
-- Profile Screen
-- ---------------------------------------------------------------------------

viewProfile :: Model -> View Model Action
viewProfile m =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-md"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "Profile" ]
        , case mProfile m of
            Nothing ->
              H.div_
                [ HP.class_ "text-center text-muted-foreground" ]
                [ text "Loading..." ]
            Just profile ->
              H.div_
                [ HP.class_ "flex flex-col gap-3" ]
                [ summaryRow ("Username", pUsername profile)
                , summaryRow ("Display Name", maybe "-" id (pDisplayName profile))
                , summaryRow ("Rating", formatRating (pRating profile) (pRatingRd profile))
                , summaryRow ("Rated Games", ms (show (pGamesRated profile)))
                , summaryRow ("Games Played", ms (show (length (mPastGames m))))
                , let wins = length $ filter (\gr -> grWinner gr /= Nothing) (mPastGames m)
                      total = length (mPastGames m)
                      rate = if total > 0 then show (wins * 100 `div` total) <> "%" else "-"
                  in summaryRow ("Win Rate", ms rate)
                , H.button_
                    [ HP.class_ "btn w-full mt-2"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick GotoProfileEdit
                    ]
                    [ text "Edit Profile" ]
                ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Profile Edit Screen
-- ---------------------------------------------------------------------------

viewProfileEdit :: Model -> View Model Action
viewProfileEdit m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "Edit Profile" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit SubmitProfileEdit
            ]
            [ H.label_
                [ HP.class_ "text-sm text-muted-foreground" ]
                [ text "Username" ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "text"
                , HP.required_ True
                , HP.value_ (mEditUsername m)
                , HP.placeholder_ "username"
                , H.onInput SetEditUsername
                ]
            , H.label_
                [ HP.class_ "text-sm text-muted-foreground" ]
                [ text "Display Name" ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "text"
                , HP.value_ (mEditDisplayName m)
                , HP.placeholder_ "Display Name"
                , H.onInput SetEditDisplayName
                ]
            , case m ^. mAuth . authError of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm" ]
                  [ text err ]
            , H.button_
                [ HP.class_ "btn w-full"
                , style_ [("touch-action", "manipulation")]
                ]
                [ text "Save" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground" ]
            [ H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoProfile
                ]
                [ text "Cancel" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

summaryRow :: (MisoString, MisoString) -> View Model Action
summaryRow (label, val) =
  H.div_
    [ HP.class_ "flex justify-between py-1 border-b border-border text-sm"
    ]
    [ H.span_
        [ HP.class_ "text-muted-foreground"
        ]
        [ text label ]
    , H.span_
        [ HP.class_ "font-medium"
        ]
        [ text val ]
    ]

-- | Format a rating with "?" suffix if provisional (RD > 100).
formatRating :: Double -> Double -> MisoString
formatRating r rd =
  let rStr = ms (show (round r :: Int))
  in if rd > 100 then rStr <> "?" else rStr
