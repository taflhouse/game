{-# LANGUAGE OverloadedStrings #-}
module App.View.Auth (viewSignIn, viewSignUp, viewUsernameGate) where

import Miso
import Miso.CSS (style_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import App.Model
import App.Action

-- ---------------------------------------------------------------------------
-- Sign In Screen
-- ---------------------------------------------------------------------------

viewSignIn :: Model -> View Model Action
viewSignIn m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "Sign In" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit DoSignIn
            ]
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "email"
                , HP.required_ True
                , HP.value_ (mAuthEmail m)
                , HP.placeholder_ "Email"
                , H.onInput SetAuthEmail
                ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "password"
                , HP.required_ True
                , HP.value_ (mAuthPassword m)
                , HP.placeholder_ "Password"
                , H.onInput SetAuthPassword
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm"
                  ]
                  [ text err ]
            , case mAuthMessage m of
                Nothing  -> H.div_ [] []
                Just msg -> H.div_
                  [ HP.class_ "text-emerald-600 dark:text-emerald-400 text-sm"
                  ]
                  [ text msg ]
            , if mAuthLoading m
                then H.div_
                  [ HP.class_ "text-center text-muted-foreground text-sm"
                  ]
                  [ text "Loading..." ]
                else H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  ]
                  [ text "Sign In" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground"
            ]
            [ text "Don't have an account? "
            , H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoSignUp
                ]
                [ text "Sign Up" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Sign Up Screen
-- ---------------------------------------------------------------------------

viewSignUp :: Model -> View Model Action
viewSignUp m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text "Sign Up" ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit DoSignUp
            ]
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "email"
                , HP.required_ True
                , HP.value_ (mAuthEmail m)
                , HP.placeholder_ "Email"
                , H.onInput SetAuthEmail
                ]
            , H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "password"
                , HP.required_ True
                , HP.value_ (mAuthPassword m)
                , HP.placeholder_ "Password"
                , H.onInput SetAuthPassword
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm"
                  ]
                  [ text err ]
            , case mAuthMessage m of
                Nothing  -> H.div_ [] []
                Just msg -> H.div_
                  [ HP.class_ "text-emerald-600 dark:text-emerald-400 text-sm"
                  ]
                  [ text msg ]
            , if mAuthLoading m
                then H.div_
                  [ HP.class_ "text-center text-muted-foreground text-sm"
                  ]
                  [ text "Loading..." ]
                else H.button_
                  [ HP.class_ "btn w-full"
                  , style_ [("touch-action", "manipulation")]
                  ]
                  [ text "Sign Up" ]
            ]
        , H.div_
            [ HP.class_ "text-center mt-4 text-sm text-muted-foreground"
            ]
            [ text "Already have an account? "
            , H.span_
                [ HP.class_ "text-foreground underline cursor-pointer"
                , SVG.onClick GotoSignIn
                ]
                [ text "Sign In" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Username Registration Gate
-- ---------------------------------------------------------------------------

viewUsernameGate :: Model -> View Model Action
viewUsernameGate m =
  H.div_
    [ HP.class_ "flex-1 flex items-center justify-center w-full"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-sm"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "Choose Your Username" ]
        , H.p_
            [ HP.class_ "text-sm text-muted-foreground mb-4 text-center" ]
            [ text "3-20 characters, letters, numbers, and underscores only." ]
        , H.form_
            [ HP.class_ "flex flex-col gap-3"
            , H.onSubmit SubmitUsername
            ]
            [ H.input_
                [ HP.class_ "input w-full text-center"
                , HP.type_ "text"
                , HP.required_ True
                , HP.value_ (mUsernameInput m)
                , HP.placeholder_ "username"
                , H.onInput SetUsernameInput
                ]
            , case mAuthError m of
                Nothing  -> H.div_ [] []
                Just err -> H.div_
                  [ HP.class_ "text-destructive text-sm text-center" ]
                  [ text err ]
            , H.button_
                [ HP.class_ "btn w-full" ]
                [ text "Continue" ]
            ]
        ]
    ]
