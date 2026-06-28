{-# LANGUAGE OverloadedStrings #-}
module App.View.Join (viewJoin) where

import Miso
import Miso.CSS (style_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import App.Model
import App.Action

-- ---------------------------------------------------------------------------
-- Join Screen
-- ---------------------------------------------------------------------------

viewJoin :: Model -> View Model Action
viewJoin m
  | mJoinCodeInput m /= "" =
    H.div_
      [ HP.class_ "flex-1 flex items-center justify-center w-full" ]
      [ H.div_
          [ HP.class_ "text-center text-muted-foreground animate-pulse"
          , style_ [("margin-top", "4em")]
          ]
          [ text "Joining game..." ]
      ]
  | otherwise =
    H.div_
      [ HP.class_ "flex-1 flex items-center justify-center w-full"
      ]
      [ H.div_
          [ HP.class_ "card p-6 w-full max-w-sm"
          , style_ [("margin-top", "4em")]
          ]
          [ H.h2_
              [ HP.class_ "text-xl font-bold mb-4 text-center" ]
              [ text "Join a Game" ]
          , H.p_
              [ HP.class_ "text-sm text-muted-foreground mb-4 text-center" ]
              [ text "Enter the invite code shared with you to play!" ]
          , H.div_
              [ HP.class_ "flex flex-col gap-3" ]
              [ H.input_
                  [ HP.class_ "input w-full text-center"
                  , HP.type_ "text"
                  , HP.placeholder_ "Invite code"
                  , HP.value_ (mJoinCodeInput m)
                  , H.onInput SetJoinCodeInput
                  ]
              , H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick JoinMultiplayerGame
                  ]
                  [ text "Join" ]
              ]
          ]
      ]
