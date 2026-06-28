{-# LANGUAGE OverloadedStrings #-}
module App.View (viewModel) where

import Data.Maybe (isNothing)
import Miso
import Miso.CSS (style_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import App.JSON (Profile(..))
import App.Model
import App.Action
import App.Game.Model (GameProps(..), GameModel)
import App.Game.Action (GameAction)
import App.Replay.Model (ReplayProps(..), ReplayModel)
import App.Replay.Action (ReplayAction)
import App.View.Home (viewHome)
import App.View.Auth (viewSignIn, viewSignUp, viewUsernameGate)
import App.View.Config (viewConfig, viewConfigure)
import App.View.Profile (viewProfile, viewProfileEdit)
import App.View.Join (viewJoin)

-- ---------------------------------------------------------------------------
-- View: Top-level layout
-- ---------------------------------------------------------------------------

viewModel
  :: Component Model GameProps GameModel GameAction
  -> Component Model ReplayProps ReplayModel ReplayAction
  -> () -> Model -> View Model Action
viewModel gameComp replayComp _ m =
  let zen = mViewMode m == ZenView && mScreen m == ReplayScreen
  in H.div_
    [ HP.class_ "fixed inset-0 flex flex-col bg-background font-sans"
    ]
    [ if zen then text "" else viewNavbar m
    , H.div_
        [ HP.class_ (if mScreen m == HomeScreen then "flex-1" else "flex-1 overflow-y-auto overscroll-none")
        ]
        [ H.div_
            [ HP.class_ (if zen
                then "flex flex-col items-center justify-center min-h-full px-4 mx-auto w-full max-w-7xl"
                else "flex flex-col items-center min-h-full pt-8 pb-12 px-4 mx-auto w-full max-w-7xl")
            ]
            [ if mNeedsUsername m && mGuestName m == Nothing && mScreen m /= SignInScreen && mScreen m /= SignUpScreen
                then viewUsernameGate m
                else case mScreen m of
                  HomeScreen        -> viewHome m
                  SignInScreen      -> viewSignIn m
                  SignUpScreen      -> viewSignUp m
                  ConfigScreen      -> viewConfig m
                  ConfigureScreen   -> viewConfigure m
                  JoinScreen        -> viewJoin m
                  GameScreen        -> viewGameScreen gameComp m
                  ReplayScreen      -> viewReplayScreen replayComp m
                  ProfileScreen     -> viewProfile m
                  ProfileEditScreen -> viewProfileEdit m
                  LoadingScreen     -> text ""
            ]
        ]
    , viewToast m
    ]

-- | Mount the game component when init data is available.
viewGameScreen :: Component Model GameProps GameModel GameAction -> Model -> View Model Action
viewGameScreen gameComp m = case mGameInitData m of
  Just initData ->
    mountWithProps_ "game"
      (GameProps (mSession m) (mProfile m) (mGuestName m) initData)
      gameComp
  Nothing ->
    H.div_
      [ HP.class_ "text-center text-muted-foreground animate-pulse"
      , style_ [("margin-top", "4em")]
      ]
      [ text "Loading..." ]

-- | Mount the replay component when game ID is available.
viewReplayScreen :: Component Model ReplayProps ReplayModel ReplayAction -> Model -> View Model Action
viewReplayScreen replayComp m = case mReplayGameId m of
  Just gameId ->
    mountWithProps_ "replay"
      (ReplayProps gameId (mViewMode m == ZenView) (mIsFullscreen m) (mZenHint m))
      replayComp
  Nothing ->
    H.div_
      [ HP.class_ "text-center text-muted-foreground animate-pulse"
      , style_ [("margin-top", "4em")]
      ]
      [ text "Loading..." ]

viewToast :: Model -> View Model Action
viewToast m = case mToast m of
  Nothing -> text ""
  Just msg ->
    H.div_ []
      [ H.div_
          [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "9998") ]
          , SVG.onClick DismissToast
          ] []
      , H.div_
          [ HP.class_ "card px-4 py-2 text-sm text-foreground shadow-lg"
          , style_ [ ("position", "fixed"), ("bottom", "1.5rem"), ("left", "50%")
                   , ("transform", "translateX(-50%)"), ("z-index", "9999")
                   , ("user-select", "text"), ("cursor", "text")
                   ]
          ]
          [ text msg ]
      ]

-- ---------------------------------------------------------------------------
-- Navbar
-- ---------------------------------------------------------------------------

viewNavbar :: Model -> View Model Action
viewNavbar m =
  H.div_
    [ HP.class_ "border-b border-border bg-background/95 backdrop-blur shrink-0"
    ]
    [ H.div_
        [ HP.class_ "flex items-center justify-between px-4 py-3 mx-auto w-full max-w-7xl"
        ]
        [ -- Left: brand
          H.span_
            [ HP.class_ "text-xl font-bold tracking-widest text-foreground/80 cursor-pointer select-none"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GotoHome
            ]
            [ text "TAFLHOUSE" ]
        , -- Right: controls
          H.div_
            [ HP.class_ "flex items-center gap-4"
            ]
            (themeToggleBtn : navAuthButtons m)
        ]
    ]

themeToggleBtn :: View Model Action
themeToggleBtn =
  H.button_
    [ HP.class_ "p-2 rounded-md text-foreground hover:bg-muted cursor-pointer"
    , style_ [("touch-action", "manipulation"), ("background", "none"), ("border", "none")]
    , SVG.onClick ToggleTheme
    , HP.title_ "Toggle theme"
    ]
    [ iconSun, iconMoon ]

iconSun :: View Model Action
iconSun =
  SVG.svg_
    [ HP.class_ "hidden dark:block"
    , SP.viewBox_ "0 0 24 24"
    , HP.width_ "18"
    , HP.height_ "18"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.circle_ [ SP.cx_ "12", SP.cy_ "12", SP.r_ "4" ]
    , SVG.path_ [ SP.d_ "M12 2v2" ]
    , SVG.path_ [ SP.d_ "M12 20v2" ]
    , SVG.path_ [ SP.d_ "m4.93 4.93 1.41 1.41" ]
    , SVG.path_ [ SP.d_ "m17.66 17.66 1.41 1.41" ]
    , SVG.path_ [ SP.d_ "M2 12h2" ]
    , SVG.path_ [ SP.d_ "M20 12h2" ]
    , SVG.path_ [ SP.d_ "m6.34 17.66-1.41 1.41" ]
    , SVG.path_ [ SP.d_ "m19.07 4.93-1.41 1.41" ]
    ]

iconMoon :: View Model Action
iconMoon =
  SVG.svg_
    [ HP.class_ "dark:hidden"
    , SP.viewBox_ "0 0 24 24"
    , HP.width_ "18"
    , HP.height_ "18"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z" ] ]

navAuthButtons :: Model -> [View Model Action]
navAuthButtons m =
    (case mSession m of
      Just _ | isNothing (mGuestName m) ->
        [ H.div_
            [ style_ [("position", "relative")] ]
            [ H.span_
                [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer flex items-center gap-1"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick ToggleProfileDropdown
                ]
                [ -- User icon
                  SVG.svg_
                    [ SP.viewBox_ "0 0 24 24"
                    , HP.width_ "18"
                    , HP.height_ "18"
                    , SP.fill_ "none"
                    , SP.stroke_ "currentcolor"
                    , SP.strokeWidth_ "2"
                    , SP.strokeLinecap_ "round"
                    , SP.strokeLinejoin_ "round"
                    ]
                    [ SVG.path_ [ SP.d_ "M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2" ]
                    , SVG.circle_ [ SP.cx_ "12", SP.cy_ "7", SP.r_ "4" ]
                    ]
                , H.span_
                    [ HP.class_ "hidden sm:inline" ]
                    [ text (maybe "" pUsername (mProfile m)) ]
                ]
            , if mProfileDropdown m
                then H.div_
                  [ HP.class_ "card p-2 flex flex-col gap-1"
                  , style_ [ ("position", "absolute"), ("right", "0"), ("top", "100%")
                           , ("margin-top", "0.5em"), ("min-width", "8rem"), ("z-index", "50")
                           ]
                  ]
                  [ H.button_
                      [ HP.class_ "text-sm text-left px-3 py-1.5 rounded hover:bg-muted cursor-pointer bg-transparent border-0 text-foreground w-full"
                      , style_ [("touch-action", "manipulation")]
                      , SVG.onClick GotoProfile
                      ]
                      [ text "Profile" ]
                  , H.button_
                      [ HP.class_ "text-sm text-left px-3 py-1.5 rounded hover:bg-muted cursor-pointer bg-transparent border-0 text-foreground w-full"
                      , style_ [("touch-action", "manipulation")]
                      , SVG.onClick DoSignOut
                      ]
                      [ text "Logout" ]
                  ]
                else text ""
            ]
        ]
      _ ->
        [ H.span_
            [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GotoSignIn
            ]
            [ text "Sign In" ]
        ]
    )
