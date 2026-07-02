{-# LANGUAGE OverloadedStrings #-}
module App.View.Join (viewJoin) where

import Miso
import Miso.CSS (style_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import App.JSON (Profile(..))
import App.Model
import App.Action

-- ---------------------------------------------------------------------------
-- Join Screen
-- ---------------------------------------------------------------------------

viewJoin :: Model -> View Model Action
viewJoin m
  | Just _ <- mPendingRatedJoin m =
    -- Rated game sign-in prompt
    H.div_
      [ HP.class_ "flex-1 flex items-center justify-center w-full" ]
      [ H.div_
          [ HP.class_ "card p-6 w-full max-w-sm"
          , style_ [("margin-top", "4em")]
          ]
          [ H.h2_
              [ HP.class_ "text-xl font-bold mb-4 text-center" ]
              [ text "Rated Game" ]
          , H.p_
              [ HP.class_ "text-sm text-muted-foreground mb-4 text-center" ]
              [ text "This is a rated game. Sign in to have it count toward your rating." ]
          , H.div_
              [ HP.class_ "flex flex-col gap-2" ]
              [ H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick JoinRatedWithSignIn
                  ]
                  [ text "Sign In" ]
              , H.button_
                  [ HP.class_ "btn btn-outline text-foreground w-full"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick JoinRatedAsGuest
                  ]
                  [ text "Continue as Guest" ]
              ]
          ]
      ]
  | mDeferredMpAction m == Just DeferJoin =
    -- Waiting for anonymous sign-in
    joiningSpinner
  | mJoinCodeInput m /= "" && not needsName =
    -- Has code and name, auto-joining
    joiningSpinner
  | otherwise =
    -- Show form (name input when needed, code input when missing)
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
              [ text (if needsName then "Enter your name to join!" else "Enter the invite code shared with you to play!") ]
          , H.div_
              [ HP.class_ "flex flex-col gap-3" ]
              (  [ H.input_
                     [ HP.class_ "input w-full text-center"
                     , HP.type_ "text"
                     , HP.placeholder_ "Your name"
                     , HP.value_ (mJoinNameInput m)
                     , H.onInput SetJoinNameInput
                     ]
                 | needsName ]
              ++ [ H.input_
                     [ HP.class_ "input w-full text-center"
                     , HP.type_ "text"
                     , HP.placeholder_ "Invite code"
                     , HP.value_ (mJoinCodeInput m)
                     , H.onInput SetJoinCodeInput
                     ]
                 | mJoinCodeInput m == "" ]
              ++ [ H.button_
                     ([ HP.class_ "btn w-full"
                      , style_ [("touch-action", "manipulation")]
                      , SVG.onClick JoinMultiplayerGame
                      ] ++ [ HP.disabled_ | joinDisabled ])
                     [ text "Join" ]
                 ]
              )
          ]
      ]
  where
    needsName = case mProfile m of
      Just p | pUsername p /= "" -> False
      _ -> mGuestName m == Nothing
    joinDisabled = (needsName && mJoinNameInput m == "")
               || mJoinCodeInput m == ""
    joiningSpinner =
      H.div_
        [ HP.class_ "flex-1 flex items-center justify-center w-full" ]
        [ H.div_
            [ HP.class_ "text-center text-muted-foreground animate-pulse"
            , style_ [("margin-top", "4em")]
            ]
            [ text "Joining game..." ]
        ]
