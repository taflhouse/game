{-# LANGUAGE OverloadedStrings #-}
module App.View (viewModel) where

import Data.Maybe (fromMaybe, isNothing)
import Miso hiding ((!!))
import Miso.CSS (style_)
import Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import qualified Data.Text as T

import Tafl.Board
import Tafl.Rules (BoardVariant(..), RuleSet(..))
import Tafl.Game.State

import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import App.JSON
import App.Model
import App.Action
import App.FFI (js_formatDeadline, js_formatDate)

-- ---------------------------------------------------------------------------
-- View: Top-level layout
-- ---------------------------------------------------------------------------

viewModel :: () -> Model -> View Model Action
viewModel _ m =
  let zen = mViewMode m == ZenView && mScreen m `elem` [GameScreen, ReplayScreen]
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
                  HomeScreen    -> viewHome m
                  SignInScreen  -> viewSignIn m
                  SignUpScreen  -> viewSignUp m
                  ConfigScreen  -> viewConfig m
                  ConfigureScreen -> viewConfigure m
                  JoinScreen    -> viewJoin m
                  GameScreen    -> viewGame m
                  ReplayScreen  -> viewReplay m
                  ProfileScreen     -> viewProfile m
                  ProfileEditScreen -> viewProfileEdit m
                  LoadingScreen     -> text ""
            ]
        ]
    , viewToast m
    , viewZenHint m
    ]

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
            (fullscreenToggleBtn m : themeToggleBtn : navAuthButtons m)
        ]
    ]

fullscreenToggleBtn :: Model -> View Model Action
fullscreenToggleBtn m
  | mScreen m `elem` [GameScreen, ReplayScreen] =
    H.button_
      [ HP.class_ "p-2 rounded-md text-foreground hover:bg-muted cursor-pointer"
      , style_ [("touch-action", "manipulation"), ("background", "none"), ("border", "none")]
      , SVG.onClick ToggleFullscreen
      , HP.title_ "Fullscreen"
      ]
      [ SVG.svg_
          [ SP.viewBox_ "0 0 24 24"
          , HP.width_ "18"
          , HP.height_ "18"
          , SP.fill_ "none"
          , SP.stroke_ "currentcolor"
          , SP.strokeWidth_ "2"
          , SP.strokeLinecap_ "round"
          , SP.strokeLinejoin_ "round"
          ]
          [ SVG.path_ [ SP.d_ "M8 3H5a2 2 0 0 0-2 2v3" ]
          , SVG.path_ [ SP.d_ "M21 8V5a2 2 0 0 0-2-2h-3" ]
          , SVG.path_ [ SP.d_ "M3 16v3a2 2 0 0 0 2 2h3" ]
          , SVG.path_ [ SP.d_ "M16 21h3a2 2 0 0 0 2-2v-3" ]
          ]
      ]
  | otherwise = text ""

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
    homeContent = case mSession m of
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
-- Config Screen
-- ---------------------------------------------------------------------------

viewConfig :: Model -> View Model Action
viewConfig _ =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center"
    ]
    [ H.div_
        [ HP.class_ "w-full max-w-md flex flex-col gap-3"
        , style_ [("margin-top", "4em")]
        ]
        [ modeLargeBtn (SetGameMode PracticeMode) "Single Player vs Self"
        , modeLargeBtn (SetGameMode AiMode) "Single Player vs AI"
        , modeLargeBtn (SetGameMode MultiplayerMode) "Multiplayer"
        ]
    ]

modeLargeBtn :: Action -> MisoString -> View Model Action
modeLargeBtn action label =
  H.button_
    [ HP.class_ "btn btn-outline text-foreground w-full py-3 font-bold"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

viewConfigure :: Model -> View Model Action
viewConfigure m =
  let modeText = case mGameMode m of
        PracticeMode    -> "Single Player vs Self"
        AiMode          -> "Single Player vs AI"
        MultiplayerMode -> "Multiplayer"
  in H.div_
    [ HP.class_ "w-full flex flex-col items-center"
    ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-md"
        , style_ [("margin-top", "4em")]
        ]
        [ H.h2_
            [ HP.class_ "text-xl font-bold mb-4 text-center"
            ]
            [ text modeText ]
        , setupSection "Board"
            [ setupBtn (SetVariant Brandubh) "Brandubh 7x7" (mVariant m == Brandubh)
            , setupBtn (SetVariant Tablut) "Tablut 9x9" (mVariant m == Tablut)
            , setupBtn (SetVariant Classic) "Copenhagen 11x11" (mVariant m == Classic)
            , setupBtn (SetVariant Parlett) "Parlett 13x13" (mVariant m == Parlett)
            , setupBtn (SetVariant DamienWalker) "Damien Walker 15x15" (mVariant m == DamienWalker)
            ]
        , if mGameMode m == AiMode then viewSetupAi m
          else if mGameMode m == MultiplayerMode then viewSetupMultiplayer m
          else H.div_ [] []
        , H.div_
            [ HP.class_ "mt-4 flex flex-col items-center gap-2"
            ]
            [ if mGameMode m == MultiplayerMode
                then H.button_
                  [ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick CreateMultiplayerGame
                  ]
                  [ text "Create" ]
                else H.button_
                  [ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick StartGame
                  ]
                  [ text "Start" ]
            , H.span_
                [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick GotoConfig
                ]
                [ text "Back" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Setup helpers (shared by config options and profile)
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

setupSection :: MisoString -> [View Model Action] -> View Model Action
setupSection label children =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4"
        ]
        [ text label ]
    , H.div_
        [ HP.class_ "flex gap-2 flex-wrap justify-center"
        ]
        children
    ]

setupBtn :: Action -> MisoString -> Bool -> View Model Action
setupBtn action label isActive =
  H.button_
    [ HP.class_ (if isActive then "btn btn-secondary" else "btn btn-outline text-foreground")
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

setupBtnDisabled :: MisoString -> View Model Action
setupBtnDisabled label =
  H.button_
    [ HP.class_ "btn btn-outline text-muted-foreground opacity-60 cursor-not-allowed"
    , HP.disabled_
    ]
    [ text label ]

viewSetupAi :: Model -> View Model Action
viewSetupAi m =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4"
        ]
        [ text "AI SETTINGS" ]
    -- AI side
    , H.div_
        [ HP.class_ "flex gap-2 flex-wrap justify-center mb-2"
        ]
        [ setupBtn (SetAiSide AttackerSide) "AI plays Attackers" (mAiSide m == AttackerSide)
        , setupBtn (SetAiSide DefenderSide) "AI plays Defenders" (mAiSide m == DefenderSide)
        ]
    -- Depth
    , H.div_
        [ HP.class_ "text-center", style_ [("position", "relative")] ]
        [ H.div_
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4 flex items-center justify-center gap-1" ]
            [ text "DEPTH"
            , H.span_
                [ HP.class_ "inline-flex items-center justify-center w-4 h-4 rounded-full border border-muted-foreground text-[10px] cursor-pointer"
                , SVG.onClick ToggleDepthInfo
                ]
                [ text "?" ]
            ]
        , if mShowDepthInfo m
            then H.div_
              [ HP.class_ "card p-3 text-left text-sm text-muted-foreground"
              , style_ [ ("position", "absolute"), ("top", "1.5em"), ("left", "50%")
                       , ("transform", "translateX(-50%)"), ("width", "16rem"), ("z-index", "50")
                       ]
              ]
              [ text "How many moves ahead the AI looks. Higher = stronger but slower." ]
            else text ""
        , H.div_
            [ HP.class_ "flex gap-1 flex-wrap justify-center" ]
            [ setupBtn (SetAiDepth d) (ms (show d)) (mAiDepth m == d)
            | d <- [1..8]
            ]
        ]
    -- Node limit
    , H.div_
        [ HP.class_ "text-center", style_ [("position", "relative")] ]
        [ H.div_
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-4 flex items-center justify-center gap-1" ]
            [ text "NODES"
            , H.span_
                [ HP.class_ "inline-flex items-center justify-center w-4 h-4 rounded-full border border-muted-foreground text-[10px] cursor-pointer"
                , SVG.onClick ToggleNodesInfo
                ]
                [ text "?" ]
            ]
        , if mShowNodesInfo m
            then H.div_
              [ HP.class_ "card p-3 text-left text-sm text-muted-foreground"
              , style_ [ ("position", "absolute"), ("top", "1.5em"), ("left", "50%")
                       , ("transform", "translateX(-50%)"), ("width", "16rem"), ("z-index", "50")
                       ]
              ]
              [ text "Max positions the AI evaluates per move. Caps search time. 'None' = unlimited." ]
            else text ""
        , H.div_
            [ HP.class_ "flex gap-1 flex-wrap justify-center" ]
            [ setupBtn (SetAiNodeLimit n) label (mAiNodeLimit m == n)
            | (n, label) <- [ (1000, "1K"), (5000, "5K")
                            , (10000, "10K"), (50000, "50K"), (100000, "100K")
                            , (0, "None")
                            ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Multiplayer Setup (inside config options)
-- ---------------------------------------------------------------------------

viewSetupMultiplayer :: Model -> View Model Action
viewSetupMultiplayer m =
  H.div_
    [ HP.class_ "text-center" ]
    [ setupSection "Your Side"
        [ setupBtn (SetSidePreference "attacker") "Attackers" (mSidePreference m == "attacker")
        , setupBtn (SetSidePreference "defender") "Defenders" (mSidePreference m == "defender")
        ]
    , setupSection "Time Control"
        [ setupBtn (SetTimeControl NoTimeControl) "None" (mTimeControl m == NoTimeControl)
        , setupBtn (SetTimeControl (BlitzControl 300000)) "Blitz" (isBlitz (mTimeControl m))
        , setupBtn (SetTimeControl (DailyControl 86400)) "Daily" (isDaily (mTimeControl m))
        ]
    , case mTimeControl m of
        BlitzControl _ ->
          setupSection "Time Per Player"
            [ setupBtn (SetTimeControl (BlitzControl 120000)) "2 min" (mTimeControl m == BlitzControl 120000)
            , setupBtn (SetTimeControl (BlitzControl 300000)) "5 min" (mTimeControl m == BlitzControl 300000)
            , setupBtn (SetTimeControl (BlitzControl 600000)) "10 min" (mTimeControl m == BlitzControl 600000)
            ]
        DailyControl _ ->
          setupSection "Time Per Move"
            [ setupBtn (SetTimeControl (DailyControl 86400)) "1 day" (mTimeControl m == DailyControl 86400)
            , setupBtn (SetTimeControl (DailyControl 172800)) "2 days" (mTimeControl m == DailyControl 172800)
            , setupBtn (SetTimeControl (DailyControl 259200)) "3 days" (mTimeControl m == DailyControl 259200)
            ]
        NoTimeControl -> text ""
    ]

isBlitz :: TimeControl -> Bool
isBlitz (BlitzControl _) = True
isBlitz _                = False

isDaily :: TimeControl -> Bool
isDaily (DailyControl _) = True
isDaily _                = False

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
            , case mAuthError m of
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
-- Game Screen
-- ---------------------------------------------------------------------------

viewGame :: Model -> View Model Action
viewGame m
  -- Show waiting screen when multiplayer game waiting for opponent
  | mGameMode m == MultiplayerMode, Nothing <- mOpponentName m =
    H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ H.div_
          [ HP.class_ "card p-6 w-full max-w-md text-center"
          , style_ [("margin-top", "4em")]
          ]
          [ H.h2_
              [ HP.class_ "text-xl font-bold mb-4" ]
              [ text "Waiting for opponent..." ]
          , H.div_
              [ HP.class_ "animate-pulse text-muted-foreground mb-4" ]
              [ text "Share the invite link to start" ]
          , case mInviteCode m of
              Just code -> H.div_
                [ HP.class_ "flex flex-col gap-4 items-center" ]
                (  [ case mQrDataUrl m of
                       Just qr -> H.img_
                         [ HP.src_ qr
                         , HP.width_ "200"
                         , HP.height_ "200"
                         , HP.class_ "rounded"
                         ]
                       Nothing -> text ""
                   , H.button_
                       [ HP.class_ "btn btn-outline text-foreground"
                       , style_ [("touch-action", "manipulation")]
                       , SVG.onClick (CopyInviteCode code)
                       ]
                       [ text "Copy Link" ]
                   ]
                )
              Nothing -> text ""
          ]
      ]
  | otherwise =
    let zen = mViewMode m == ZenView
        showEval = mGameMode m /= MultiplayerMode
        showClocks = not zen && mTimeControl m /= NoTimeControl && mGameMode m == MultiplayerMode
        n = boardSize (gsBoard (mGameState m))
        -- Determine which clocks go on top/bottom based on player perspective
        myName = case mGuestName m of
          Just gn -> gn
          Nothing -> maybe "You" pUsername (mProfile m)
        (topName, topMs, topIsActive, botName, botMs, botIsActive) = case mPlayerSide m of
          Just AttackerSide ->
            ( fromMaybe "Opponent" (mOpponentName m), mDefenderTimeMs m, turnSide (mGameState m) == DefenderSide
            , myName, mAttackerTimeMs m, turnSide (mGameState m) == AttackerSide )
          _ ->
            ( fromMaybe "Opponent" (mOpponentName m), mAttackerTimeMs m, turnSide (mGameState m) == AttackerSide
            , myName, mDefenderTimeMs m, turnSide (mGameState m) == DefenderSide )
    in H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ if showClocks then viewClock n topName topMs topIsActive (mTimeControl m) (mMoveDeadline m) True else text ""
      , H.div_
          -- margin-top set via #board-row in styles.css (reduced in fullscreen+zen on small screens)
          [ HP.id_ "board-row"
          , HP.class_ ("flex flex-row items-stretch justify-center gap-2" <> if zen then " zen" else "")
          ]
          [ if showEval && not zen then viewEvalBar m else text ""
          , viewBoardPanel m
          ]
      , if showClocks then viewClock n botName botMs botIsActive (mTimeControl m) (mMoveDeadline m) False else text ""
      , if zen then text "" else viewStatus m
      , if zen then text "" else viewMoveHistory m
      , if zen then text ""
        else if mGameMode m == MultiplayerMode then viewMultiplayerControls m else text ""
      , if zen then text "" else viewShareLink m
      ]

-- ---------------------------------------------------------------------------
-- Clock Display
-- ---------------------------------------------------------------------------

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

-- | Format a daily deadline as relative time.
formatDeadline :: MisoString -> MisoString
formatDeadline _deadline = "deadline set"  -- simplified; real relative time needs JS

-- | Render a clock row above or below the board.
viewClock :: Int -> MisoString -> Int -> Bool -> TimeControl -> Maybe MisoString -> Bool -> View Model Action
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

-- ---------------------------------------------------------------------------
-- Replay Screen
-- ---------------------------------------------------------------------------

viewReplay :: Model -> View Model Action
viewReplay m
  | mReplayNotFound m =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8"
      ]
      [ text "This game is private or doesn't exist." ]
  | Nothing <- mReplayGame m =
    H.div_
      [ HP.class_ "text-center text-muted-foreground mt-8 animate-pulse"
      ]
      [ text "Loading game..." ]
  | Just gr <- mReplayGame m =
    let zen = mViewMode m == ZenView
    in H.div_
      [ HP.class_ "w-full flex flex-col items-center"
      ]
      [ if zen then text "" else viewReplayHeader gr
      , case mReplayStates m of
          [] -> H.div_
            [ HP.class_ "card p-6 text-center mt-4"
            ]
            [ text "No move data available for this game." ]
          states ->
            let gs = states !! mReplayIndex m
                n = boardSize (gsBoard gs)
            in H.div_
              [ HP.class_ "w-full flex flex-col items-center"
              ]
              [ H.div_
                  [ HP.class_ "flex flex-row items-stretch justify-center gap-2"
                  , style_ [("margin-top", "1em")]
                  ]
                  [ if not zen then viewEvalBar m else text ""
                  , viewReplayBoardPanel m gs
                  ]
              , viewReplayControls m n
              , if zen then text "" else viewReplayMoveList m n
              ]
      ]

viewReplayHeader :: GameRecord -> View Model Action
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

viewReplayBoardPanel :: Model -> GameState -> View Model Action
viewReplayBoardPanel m gs =
  let n = boardSize (gsBoard gs)
      totalPx = sqSize * n
      fs = mIsFullscreen m
      zen = mViewMode m == ZenView
      fsSize = if zen
        then "85vmin"
        else "clamp(50vmin, calc(100vh - 29rem), 85vmin)"
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border"
    , style_ (if fs
        then [("width", fsSize), ("height", fsSize)]
        else [("width", ms totalPx <> "px"), ("max-width", "calc(100vw - 3rem)")])
    ]
    [ viewReplaySVGBoard gs ]

viewReplaySVGBoard :: GameState -> View Model Action
viewReplaySVGBoard gs =
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
    ++ [ renderPiece n r c (pieceAt board (Coords r c))
       | r <- [0..n-1], c <- [0..n-1]
       , pieceAt board (Coords r c) /= Empty
       ]
    ++ renderReplayLastMove gs n
    )

renderReplayLastMove :: GameState -> Int -> [View Model Action]
renderReplayLastMove gs _n = case gsLastAction gs of
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

viewReplayControls :: Model -> Int -> View Model Action
viewReplayControls m n =
  let idx = mReplayIndex m
      maxIdx = length (mReplayStates m) - 1
  in H.div_
    [ HP.class_ "flex items-center justify-center gap-2 my-4 w-full"
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ replayBtn (ReplayGotoMove 0) "|<" (idx > 0)
    , replayBtn (ReplayGotoMove (idx - 1)) "<" (idx > 0)
    , H.span_
        [ HP.class_ "text-sm font-mono text-muted-foreground min-w-[5em] text-center" ]
        [ text (ms (show idx) <> " / " <> ms (show maxIdx)) ]
    , replayBtn (ReplayGotoMove (idx + 1)) ">" (idx < maxIdx)
    , replayBtn (ReplayGotoMove maxIdx) ">|" (idx < maxIdx)
    , replayBtn ToggleZenMode "Zen" True
    ]

replayBtn :: Action -> MisoString -> Bool -> View Model Action
replayBtn action label enabled =
  H.button_
    [ HP.class_ (if enabled
        then "btn btn-outline btn-sm text-foreground"
        else "btn btn-outline btn-sm text-muted-foreground opacity-50 cursor-not-allowed")
    , style_ [("touch-action", "manipulation"), ("min-width", "2.5em")]
    , SVG.onClick (if enabled then action else NoOp)
    ]
    [ text label ]

viewReplayMoveList :: Model -> Int -> View Model Action
viewReplayMoveList m n =
  case grMoves =<< mReplayGame m of
    Nothing -> H.div_ [] []
    Just moves | null moves -> H.div_ [] []
    Just moves ->
      let states = mReplayStates m
          idx = mReplayIndex m
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

replayMoveBtn :: Int -> MoveAction -> Int -> Bool -> [GameState] -> View Model Action
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
    , SVG.onClick (ReplayGotoMove idx)
    ]
    [ text la ]

-- ---------------------------------------------------------------------------
-- Board
-- ---------------------------------------------------------------------------

sqSize :: Int
sqSize = 54

-- | Evaluation bar shown left of the board. Positive = attackers favored, negative = defenders.
viewEvalBar :: Model -> View Model Action
viewEvalBar m =
  let score = mEvalScore m
      -- Clamp score to [-1500, 1500], then map to attacker % (0-100)
      clamped = max (-1500) (min 1500 score)
      attackerPct = 50.0 + fromIntegral clamped / 1500.0 * 50.0 :: Double
      defenderPct = 100.0 - attackerPct :: Double
      -- Display score divided by 100 for readability
      displayScore = if score >= 0
        then "+" <> ms (showScore score)
        else ms (showScore score)
  in H.div_
    [ HP.class_ "flex flex-col rounded overflow-hidden border border-border"
    , style_ [ ("width", "20px"), ("flex-shrink", "0"), ("position", "relative") ]
    ]
    [ -- Attacker portion (top)
      H.div_
        [ HP.class_ "w-full transition-all duration-300"
        , style_ [ ("height", ms (showPct attackerPct) <> "%")
                 , ("background", "var(--piece-attacker)") ]
        ] []
    , -- Defender portion (bottom)
      H.div_
        [ HP.class_ "w-full transition-all duration-300"
        , style_ [ ("height", ms (showPct defenderPct) <> "%")
                 , ("background", "var(--piece-defender)") ]
        ] []
    , -- Score label overlay
      H.div_
        [ HP.class_ "absolute text-center"
        , style_ [ ("font-size", "9px"), ("line-height", "1"), ("width", "20px")
                 , ("top", "50%"), ("transform", "translateY(-50%)")
                 , ("color", "var(--muted-foreground)"), ("pointer-events", "none")
                 , ("mix-blend-mode", "difference"), ("font-weight", "bold") ]
        ]
        [ text displayScore ]
    ]

showScore :: Int -> String
showScore s =
  let (q, r) = abs s `divMod` 100
      sign = if s < 0 then "-" else ""
  in sign ++ show q ++ "." ++ (if r < 10 then "0" else "") ++ show r

showPct :: Double -> String
showPct d =
  let n = round d :: Int
  in show n

-- | The game state currently being displayed (browsed or live).
displayedGameState :: Model -> GameState
displayedGameState m = case mBrowseIndex m of
  Nothing -> mGameState m
  Just i  -> let allStates = mHistory m ++ [mGameState m]
             in if i >= 0 && i < length allStates
                then allStates !! i
                else mGameState m

viewBoardPanel :: Model -> View Model Action
viewBoardPanel m =
  let m' = m { mGameState = displayedGameState m }
      n = boardSize (gsBoard (mGameState m'))
      totalPx = sqSize * n
      fs = mIsFullscreen m
      zen = mViewMode m == ZenView
      fsSize = if zen
        then "85vmin"
        else "clamp(50vmin, calc(100vh - 29rem), 85vmin)"
  in H.div_
    [ HP.class_ "relative shadow-2xl rounded overflow-hidden border-2 border-border"
    , style_ (if fs
        then [("width", fsSize), ("height", fsSize)]
        else [("width", ms totalPx <> "px"), ("max-width", "calc(100vw - 3rem)")])
    ]
    [ viewSVGBoard m' ]

viewSVGBoard :: Model -> View Model Action
viewSVGBoard m =
  let gs    = mGameState m
      board = gsBoard gs
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

-- Square background colors (themed via CSS variables)
renderSquareBg :: Int -> Int -> Int -> View Model Action
renderSquareBg _n r c =
  SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , style_ [("fill", if even (r + c) then "var(--muted)" else "var(--accent)")]
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
  in map (\pos -> markSquare pos "var(--piece-king)") corners
     ++ [markSquare (center, center) "var(--piece-defender)"]

-- Highlight selected square (colored by selected piece)
renderHighlights :: Model -> Int -> [View Model Action]
renderHighlights m _n = case mSelected m of
  Nothing -> []
  Just sc@(Coords r c) ->
    let hlColor = case pieceAt (gsBoard (mGameState m)) sc of
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 45%, transparent)"
          Defender -> "color-mix(in oklch, var(--piece-defender) 45%, transparent)"
          King     -> "color-mix(in oklch, var(--piece-king) 45%, transparent)"
          _        -> "rgba(80,200,120,0.45)"
    in [ SVG.rect_
        [ SP.x_ (ms (c * sqSize))
        , SP.y_ (ms (r * sqSize))
        , HP.width_ (ms sqSize)
        , HP.height_ (ms sqSize)
        , SP.fill_ hlColor
        ]
    ]

-- Valid move dots (colored by selected piece)
renderValidDots :: Model -> Int -> [View Model Action]
renderValidDots m _n =
  let dotColor = case mSelected m of
        Nothing -> "rgba(80,200,120,0.6)"
        Just sc -> case pieceAt (gsBoard (mGameState m)) sc of
          Attacker -> "color-mix(in oklch, var(--piece-attacker) 60%, transparent)"
          Defender -> "color-mix(in oklch, var(--piece-defender) 60%, transparent)"
          King     -> "color-mix(in oklch, var(--piece-king) 60%, transparent)"
          _        -> "rgba(80,200,120,0.6)"
  in [ SVG.circle_
      [ SP.cx_ (ms (col coord * sqSize + sqSize `div` 2))
      , SP.cy_ (ms (row coord * sqSize + sqSize `div` 2))
      , SP.r_ (ms (sqSize `div` 5))
      , SP.fill_ dotColor
      ]
  | coord <- mValidMoves m
  ]

-- Last move indicators (colored by the side that moved)
renderLastMove :: Model -> Int -> [View Model Action]
renderLastMove m _n = case gsLastAction (mGameState m) of
  Nothing -> []
  Just (MoveAction f t) ->
    let gs = mGameState m
        movedPiece = pieceAt (gsBoard gs) t
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

-- Piece rendering
renderPiece :: Int -> Int -> Int -> Piece -> View Model Action
renderPiece _n r c piece =
  let cx = c * sqSize + sqSize `div` 2
      cy = r * sqSize + sqSize `div` 2
      radius = sqSize `div` 2 - 4
      (fill, stroke, label) = case piece of
        Attacker -> ("var(--piece-attacker)", "var(--border)", "A" :: MisoString)
        Defender -> ("var(--piece-defender)", "var(--border)", "D")
        King     -> ("var(--piece-king)", "var(--piece-king-stroke)", "K")
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
        , SP.fill_ (case piece of
            Attacker -> "var(--piece-attacker-fg)"
            King     -> "var(--piece-king-fg)"
            Defender -> "var(--piece-defender-fg)"
            _        -> "#333")
        , SP.fontFamily_ "Arial, sans-serif"
        ]
        [ text label ]
    ]

-- Transparent click targets
renderClickTarget :: Model -> Int -> Int -> Int -> View Model Action
renderClickTarget m _n r c =
  let gs = mGameState m
      side = turnSide gs
      aiBlocked = mGameMode m == AiMode && mAiSide m == side
      mpBlocked = mGameMode m == MultiplayerMode && mPlayerSide m /= Just side
      blocked = mAiThinking m || aiBlocked || mpBlocked || finished (gsResult gs)
      cur = if blocked then "default" else "pointer"
  in SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ "transparent"
    , style_ [("cursor", cur), ("touch-action", "manipulation")]
    , SVG.onClick (CellClicked (Coords r c))
    ]

-- ---------------------------------------------------------------------------
-- Status & Controls
-- ---------------------------------------------------------------------------

viewStatus :: Model -> View Model Action
viewStatus m =
  let gs     = mGameState m
      n      = boardSize (gsBoard gs)
      result = gsResult gs
      side   = turnSide gs
      caps   = gsCaptures gs
      isAi   = mGameMode m == AiMode
      isMp   = mGameMode m == MultiplayerMode
      myTurn = mPlayerSide m == Just side
      baseCls = "text-center my-4 font-bold card px-3 w-full flex justify-center items-center"
      (cls, msg)
        | finished result = case winner result of
            Just AttackerSide -> (baseCls <> " text-destructive", "Attackers win! " <> desc result)
            Just DefenderSide -> (baseCls, "Defenders win! " <> desc result)
            Nothing           -> (baseCls, "Draw! " <> desc result)
        | mAiThinking m = (baseCls <> " text-muted-foreground animate-pulse", "AI thinking...")
        | isAi && mAiSide m == side = (baseCls,
            (if side == AttackerSide then "Attacker's turn" else "Defender's turn") <> " (AI)")
        | isAi = (baseCls, "Your turn")
        | isMp && myTurn = (baseCls, "Your turn")
        | isMp = (baseCls <> " text-muted-foreground",
            maybe "Opponent" fromMisoString (mOpponentName m) <> "'s turn")
        | side == AttackerSide = (baseCls, "Attacker's turn")
        | otherwise            = (baseCls, "Defender's turn")
      borderColor
        | not (finished result) = "transparent"
        | otherwise = case winner result of
            Just AttackerSide -> "var(--piece-attacker)"
            Just DefenderSide -> "var(--piece-defender)"
            _                 -> "var(--muted-foreground)"
      capSuffix :: T.Text
      capSuffix
        | finished result || null caps = ""
        | otherwise = let c = length caps
                      in " · Captured " <> T.pack (show c) <> if c == 1 then " piece" else " pieces"
      fullMsg = msg <> capSuffix
  in H.div_
    [ HP.class_ cls
    , style_ [ ("max-width", ms (sqSize * n) <> "px")
             , ("min-height", "3.5rem")
             , ("border", "1px solid " <> borderColor)
             , ("border-radius", "0.375rem")
             ]
    ]
    [ text (ms fullMsg) ]

viewShareLink :: Model -> View Model Action
viewShareLink m =
  let result = gsResult (mGameState m)
  in if finished result
       then case (mGameId m, mSession m) of
         (Just gid, Just sess)
           | amProvider (userAppMetadata (sessionUser sess)) /= "anonymous"
             || mGameMode m == MultiplayerMode
             -> viewShareSection m gid
         _   -> text ""
       else text ""

viewShareSection :: Model -> MisoString -> View Model Action
viewShareSection m gid =
  let url = "https://taflhouse.com/games/" <> gid
      n   = boardSize (gsBoard (mGameState m))
  in H.div_
    [ HP.class_ "flex items-center gap-2 w-full mt-4"
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ H.input_
        [ HP.class_ "input input-sm text-muted-foreground bg-transparent border border-border rounded flex-1"
        , HP.readonly_ True
        , HP.value_ url
        , style_ [("font-size", "0.8rem"), ("padding", "0.4rem 0.6rem")]
        ]
    , H.button_
        [ HP.class_ "btn btn-outline btn-sm text-foreground"
        , style_ [("touch-action", "manipulation"), ("white-space", "nowrap")]
        , SVG.onClick CopyGameLink
        ]
        [ text "Copy Link" ]
    ]

viewMultiplayerControls :: Model -> View Model Action
viewMultiplayerControls m =
  let gs = mGameState m
      n = boardSize (gsBoard gs)
      -- Check final state, not viewed state when browsing history
      finalResult = case mFullHistory m of
        Just fs -> gsResult (last fs)
        Nothing -> gsResult gs
      gameOver = finished finalResult
  in if gameOver then text ""
     else H.div_
       [ HP.class_ "flex items-center justify-center gap-2 mt-4"
       , style_ [("max-width", ms (sqSize * n) <> "px")]
       ]
       ([ H.button_
            [ HP.class_ "btn btn-outline btn-sm text-foreground"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick Resign
            ]
            [ text "Resign" ]
        , if mDrawOffered m
            then H.div_
              [ HP.class_ "flex gap-1" ]
              [ H.button_
                  [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick AcceptDraw
                  ]
                  [ text "Accept Draw" ]
              , H.button_
                  [ HP.class_ "btn btn-outline btn-sm text-foreground"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick DeclineDraw
                  ]
                  [ text "Decline" ]
              ]
            else H.button_
              [ HP.class_ "btn btn-outline btn-sm text-foreground"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick OfferDraw
              ]
              [ text "Offer Draw" ]
        ] ++ case mOpponentName m of
          Just opp -> [ H.span_
            [ HP.class_ "text-sm text-muted-foreground ml-2" ]
            [ text ("vs " <> opp) ] ]
          Nothing -> [])

viewMoveHistory :: Model -> View Model Action
viewMoveHistory m
  | null (mHistory m) && isNothing (mFullHistory m) =
      let n = boardSize (gsBoard (mGameState m))
      in H.div_
        [ HP.class_ "flex justify-center items-center w-full"
        , style_ [("max-width", ms (sqSize * n) <> "px"), ("margin-top", "0.5em")]
        ]
        [ ctrlBtn ToggleZenMode "Zen" ]
  | otherwise =
      let displayStates = mHistory m ++ [mGameState m]
          n = boardSize (gsBoard (mGameState m))
          viewIdx = case mBrowseIndex m of
            Just i  -> i
            Nothing -> length displayStates - 1
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
                [ text "HISTORY" ]
            , H.div_
                [ HP.class_ "flex gap-1" ]
                (  [ ctrlBtn ToggleZenMode "Zen" ]
                ++ [ ctrlBtn Undo "Undo"
                   | mGameMode m /= MultiplayerMode
                     || finished (gsResult (mGameState m))
                   ]
                )
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ moveBtn m i gs n (i == viewIdx)
            | (i, gs) <- reverse (zip [0..] displayStates)
            ]
        ]

moveBtn :: Model -> Int -> GameState -> Int -> Bool -> View Model Action
moveBtn m idx gs n isCurrent =
  let moveSide = opponentSide gs  -- side that made this move
      isHuman = case gsLastAction gs of
        Nothing -> False
        Just _  -> mGameMode m == PracticeMode || mAiSide m /= moveSide
      -- Determine piece type that moved (check destination square)
      movedPiece = case gsLastAction gs of
        Nothing            -> Empty
        Just (MoveAction _ t) -> pieceAt (gsBoard gs) t
      -- Current position: > prefix; human moves: bold; AI moves: dim
      pointer = if isCurrent then "> " else "  "
      moveLabel = case gsLastAction gs of
        Nothing -> "Start"
        Just (MoveAction f t) ->
          let sideChar = case moveSide of
                  AttackerSide -> "A"
                  DefenderSide -> "D"
          in ms (show idx) <> ". " <> sideChar <> " "
               <> ms (coordStr n f) <> "-" <> ms (coordStr n t)
      label = pointer <> moveLabel
      -- Color-code border and text by piece type for all moves
      (textColor, borderColor) = case movedPiece of
        Attacker -> ("var(--piece-attacker)", "var(--piece-attacker)")
        King     -> ("var(--piece-king)", "var(--piece-king)")
        Defender -> ("var(--piece-defender)", "var(--piece-defender)")
        Empty    -> ("var(--foreground)", "transparent")
      activeCls = " border-l-2"
      moveStyle = [("color", textColor), ("border-left-color", borderColor)]
      boldCls = if isHuman || isCurrent then " font-bold" else ""
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted cursor-pointer bg-transparent border-0 text-foreground" <> activeCls <> boldCls)
    , style_ (("touch-action", "manipulation") : moveStyle)
    , SVG.onClick (GotoMove idx)
    ]
    [ text label ]

coordStr :: Int -> Coords -> String
coordStr n (Coords r c) = [toEnum (fromEnum 'a' + c)] ++ show (n - r)


ctrlBtn :: Action -> MisoString -> View Model Action
ctrlBtn action label =
  H.button_
    [ HP.class_ "btn btn-outline btn-sm text-foreground"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

viewZenHint :: Model -> View Model Action
viewZenHint m
  | mZenHint m =
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
