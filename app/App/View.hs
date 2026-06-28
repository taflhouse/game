{-# LANGUAGE OverloadedStrings #-}
module App.View (viewModel) where

import Data.Maybe (isNothing)
import Miso
import Miso.CSS (style_)
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Tafl.Board (Side(..))
import Tafl.Rules (BoardVariant(..))

import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import App.JSON (Profile(..), GameRecord(..))
import App.Model
import App.Action
import App.FFI (js_formatDate)
import App.Game.Model (GameProps(..), GameModel)
import App.Game.Action (GameAction)
import App.Replay.Model (ReplayProps(..), ReplayModel)
import App.Replay.Action (ReplayAction)

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
-- Replay Screen (mounts sub-component)
-- ---------------------------------------------------------------------------

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

