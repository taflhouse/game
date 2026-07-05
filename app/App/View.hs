{-# LANGUAGE OverloadedStrings #-}
module App.View (viewModel) where

import Data.Maybe (isNothing)
import Miso
import Miso.String (MisoString, ms)
import Miso.CSS (style_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import App.JSON (Profile(..), GameRow(..))
import App.Model
import App.Action
import App.Game.Model (GameProps(..), GameModel)
import App.Game.Action (GameAction)
import App.Replay.Model (ReplayProps(..), ReplayModel)
import App.Replay.Action (ReplayAction)
import App.Tutorial.Model (TutorialProps(..), TutorialModel)
import App.Tutorial.Action (TutorialAction)
import App.View.Auth (viewSignIn, viewSignUp, viewUsernameGate)
import App.View.Home (viewYourGames, viewPlayerDetail)
import App.View.Config (viewConfig, viewConfigure)
import App.View.Profile (viewProfile, viewProfileEdit)
import App.View.Join (viewJoin)
import App.View.Lounge (viewLounge)

-- ---------------------------------------------------------------------------
-- View: Top-level layout
-- ---------------------------------------------------------------------------

viewModel
  :: Component Model GameProps GameModel GameAction
  -> Component Model ReplayProps ReplayModel ReplayAction
  -> Component Model TutorialProps TutorialModel TutorialAction
  -> () -> Model -> View Model Action
viewModel gameComp replayComp tutorialComp _ m =
  let zen = mViewMode m == ZenView && mScreen m == ReplayScreen
  in H.div_
    [ HP.class_ "fixed inset-0 flex flex-col bg-background font-sans"
    ]
    [ if zen then text "" else viewNavbar m
    , H.div_
        [ HP.class_ "flex-1 overflow-y-auto overscroll-none"
        ]
        [ H.div_
            [ HP.class_ (if zen
                then "flex flex-col items-center justify-center min-h-full px-4 mx-auto w-full max-w-7xl"
                else "flex flex-col items-center min-h-full pt-8 pb-12 px-4 mx-auto w-full max-w-7xl")
            ]
            [ if mNeedsUsername m && mGuestName m == Nothing && mScreen m /= SignInScreen && mScreen m /= SignUpScreen
                then viewUsernameGate m
                else case mScreen m of
                  HomeScreen        -> viewLounge m
                  SignInScreen      -> viewSignIn m
                  SignUpScreen      -> viewSignUp m
                  ConfigScreen      -> viewConfig m
                  ConfigureScreen   -> viewConfigure m
                  JoinScreen        -> viewJoin m
                  GameScreen        -> viewGameScreen gameComp m
                  ReplayScreen      -> viewReplayScreen replayComp m
                  ProfileScreen     -> viewProfile m
                  ProfileEditScreen -> viewProfileEdit m
                  YourGamesScreen   -> viewYourGames m
                  PlayerScreen      -> viewPlayerDetail m
                  LearnScreen       -> viewLearnScreen tutorialComp m
                  LoungeScreen      -> viewLounge m  -- legacy, redirects to home
                  LoadingScreen     -> text ""
            ]
        ]
    , viewToast m
    , viewMatchToast m
    , viewMatchModal m
    , viewReadyPopover m
    ]

-- | Mount the game component when init data is available.
viewGameScreen :: Component Model GameProps GameModel GameAction -> Model -> View Model Action
viewGameScreen gameComp m = case mGameInitData m of
  Just initData ->
    mountWithProps_ "game"
      (GameProps (mSession m) (mProfile m) (mGuestName m) initData (mIsRated m))
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

-- | Mount the tutorial component.
viewLearnScreen :: Component Model TutorialProps TutorialModel TutorialAction -> Model -> View Model Action
viewLearnScreen tutorialComp m =
  mountWithProps_ "tutorial"
    (TutorialProps (mTutorialLessonId m))
    tutorialComp

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
            (viewLfgToggle m : themeToggleBtn : learnLink : navAuthButtons m)
        ]
    ]

-- | "Learn" link in the navbar.
learnLink :: View Model Action
learnLink =
  H.span_
    [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick GotoLearn
    ]
    [ text "Learn" ]

-- | "Looking for game" toggle in the navbar — colored circle indicator.
viewLfgToggle :: Model -> View Model Action
viewLfgToggle m =
  let active = mMatchInterested m
      color = if active then "#22c55e" else "#f97316"
  in H.button_
    [ HP.class_ "p-2 rounded-md cursor-pointer hover:bg-muted"
    , style_ [ ("touch-action", "manipulation"), ("background", "none"), ("border", "none")
             , ("display", "inline-flex"), ("align-items", "center") ]
    , SVG.onClick ToggleMatchInterest
    , HP.title_ (if active then "Stop looking for games" else "Look for games")
    ]
    [ H.span_
        [ style_ [ ("width", "10px"), ("height", "10px"), ("border-radius", "50%")
                 , ("background", color), ("display", "inline-block") ] ]
        []
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
    case mSession m of
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
                      , SVG.onClick GotoYourGames
                      ]
                      [ text "Your Games" ]
                  , H.button_
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

-- ---------------------------------------------------------------------------
-- Match toast & modal (for passive matchmaking interest flow)
-- ---------------------------------------------------------------------------

-- | Toast notification shown when a matchmaking game is available.
-- Requires user action — no auto-dismiss.
viewMatchToast :: Model -> View Model Action
viewMatchToast m = case mMatchToast m of
  Nothing -> text ""
  Just gr ->
    H.div_
      [ HP.class_ "card px-4 py-3 shadow-lg"
      , style_ [ ("position", "fixed"), ("bottom", "1.5rem"), ("left", "50%")
               , ("transform", "translateX(-50%)"), ("z-index", "9997")
               , ("min-width", "16rem"), ("max-width", "22rem")
               ]
      ]
      [ H.div_ [ HP.class_ "text-sm font-bold mb-1" ]
          [ text (grwVariant gr <> " match available!") ]
      , H.div_ [ HP.class_ "text-xs text-muted-foreground mb-3" ]
          [ text (matchSummary gr) ]
      , H.div_ [ HP.class_ "flex gap-2" ]
          [ H.button_
              [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick (ViewMatchDetails gr)
              ]
              [ text "View Details" ]
          , H.button_
              [ HP.class_ "btn btn-outline btn-sm text-foreground"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick DismissMatchToast
              ]
              [ text "Dismiss" ]
          ]
      ]

-- | Full-screen modal showing game details for a matchmaking game.
viewMatchModal :: Model -> View Model Action
viewMatchModal m = case mMatchModal m of
  Nothing -> text ""
  Just gr ->
    H.div_ []
      [ -- Backdrop
        H.div_
          [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "9998")
                   , ("background", "rgba(0,0,0,0.5)")
                   ]
          , SVG.onClick (DeclineMatch (grwId gr))
          ] []
      , -- Card
        H.div_
          [ HP.class_ "card p-6 shadow-xl"
          , style_ [ ("position", "fixed"), ("top", "50%"), ("left", "50%")
                   , ("transform", "translate(-50%, -50%)"), ("z-index", "9999")
                   , ("min-width", "18rem"), ("max-width", "24rem")
                   ]
          ]
          [ H.h3_ [ HP.class_ "text-lg font-bold mb-4" ]
              [ text "Match Details" ]
          , H.div_ [ HP.class_ "flex flex-col gap-2 mb-4" ]
              [ detailRow "Variant" (grwVariant gr)
              , detailRow "Type" (if grwIsRated gr then "Rated" else "Casual")
              , detailRow "Time" (timeControlSummary gr)
              , case grwCreatorRating gr of
                  Just r -> detailRow "Creator Rating" (ms (show (round r :: Int)))
                  Nothing -> text ""
              , detailRow "Side Available"
                  (if grwAttackerId gr == Nothing then "Attacker"
                   else if grwDefenderId gr == Nothing then "Defender"
                   else "Unknown")
              ]
          , H.div_ [ HP.class_ "flex gap-2 justify-end" ]
              [ H.button_
                  [ HP.class_ "btn btn-outline btn-sm text-foreground"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick (DeclineMatch (grwId gr))
                  ]
                  [ text "Decline" ]
              , H.button_
                  [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick (AcceptMatch gr)
                  ]
                  [ text "Accept" ]
              ]
          ]
      ]

-- | A single key–value row for the match details modal.
detailRow :: MisoString -> MisoString -> View Model Action
detailRow label val =
  H.div_ [ HP.class_ "flex justify-between text-sm" ]
    [ H.span_ [ HP.class_ "text-muted-foreground" ] [ text label ]
    , H.span_ [ HP.class_ "font-medium" ] [ text val ]
    ]

-- | Brief summary of a game for the toast notification.
matchSummary :: GameRow -> MisoString
matchSummary gr =
  timeControlSummary gr <> " \xb7 " <> (if grwIsRated gr then "Rated" else "Casual")

-- | Human-readable time control summary from a GameRow.
timeControlSummary :: GameRow -> MisoString
timeControlSummary gr = case grwTimeControl gr of
  Just "blitz" -> case grwTimePerPlayerMs gr of
    Just msVal -> ms (show (msVal `div` 60000)) <> " min"
    Nothing    -> "Blitz"
  Just "daily" -> case grwTimePerMoveSec gr of
    Just s  -> ms (show (s `div` 3600)) <> " hr/move"
    Nothing -> "Daily"
  _ -> "No time control"

-- ---------------------------------------------------------------------------
-- Ready popover (first-click explanation)
-- ---------------------------------------------------------------------------

-- | Popover: step 0 = explanation, step 1 = filter preferences.
viewReadyPopover :: Model -> View Model Action
viewReadyPopover m = case mMatchReadyStep m of
  Nothing -> text ""
  Just 0  -> viewReadyExplain
  Just _  -> viewReadyFilters m

-- | Step 0: explain what the toggle does.
viewReadyExplain :: View Model Action
viewReadyExplain =
  H.div_ []
    [ -- Backdrop
      H.div_
        [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "9998") ]
        , SVG.onClick DismissReadyPopover
        ] []
    , -- Card
      H.div_
        [ HP.class_ "card p-4 shadow-lg"
        , style_ [ ("position", "fixed"), ("top", "3.5rem"), ("right", "4rem")
                 , ("z-index", "9999"), ("max-width", "18rem")
                 ]
        ]
        [ H.div_ [ HP.class_ "text-sm font-bold mb-2" ]
            [ text "Get notified of matches" ]
        , H.div_ [ HP.class_ "text-xs text-muted-foreground mb-3" ]
            [ text "When enabled, you'll receive a notification whenever someone creates a game matching your selected variant. You can then view the details and accept or decline." ]
        , H.div_ [ HP.class_ "flex gap-2 justify-end" ]
            [ H.button_
                [ HP.class_ "btn btn-outline btn-sm text-foreground"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick DismissReadyPopover
                ]
                [ text "Not now" ]
            , H.button_
                [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick ConfirmReadyPopover
                ]
                [ text "Enable" ]
            ]
        ]
    ]

-- | Step 1: match filter preferences.
viewReadyFilters :: Model -> View Model Action
viewReadyFilters m =
  H.div_ []
    [ -- Backdrop
      H.div_
        [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "9998") ]
        , SVG.onClick DismissReadyPopover
        ] []
    , -- Card
      H.div_
        [ HP.class_ "card p-4 shadow-lg"
        , style_ [ ("position", "fixed"), ("top", "3.5rem"), ("right", "4rem")
                 , ("z-index", "9999"), ("min-width", "16rem"), ("max-width", "20rem")
                 ]
        ]
        [ H.div_ [ HP.class_ "text-sm font-bold mb-3" ]
            [ text "Match preferences" ]
        -- "Any" override
        , H.label_
            [ HP.class_ "flex items-center gap-2 cursor-pointer mb-3"
            , style_ [("touch-action", "manipulation")]
            ]
            [ H.input_
                [ HP.type_ "checkbox"
                , HP.class_ "accent-green-600"
                , HP.checked_ (mMatchAny m)
                , SVG.onClick (SetMatchAny (not (mMatchAny m)))
                ]
            , H.span_ [ HP.class_ "text-sm" ] [ text "Accept any match" ]
            ]
        -- Segmented controls (dimmed when "Any" is checked)
        , H.div_
            [ HP.class_ (if mMatchAny m then "opacity-40 pointer-events-none" else "")
            ]
            [ -- Rated / Casual / Either
              filterSection "Type"
                [ ("Rated", "rated"), ("Casual", "casual"), ("Either", "either") ]
                (mMatchWantRated m)
                SetMatchWantRated
            -- Timed / Untimed / Either
            , filterSection "Time"
                [ ("Timed", "timed"), ("Untimed", "untimed"), ("Either", "either") ]
                (mMatchWantTimed m)
                SetMatchWantTimed
            -- Side preference
            , filterSection "Side"
                [ ("Attacker", "attacker"), ("Defender", "defender"), ("Either", "either") ]
                (mMatchWantSide m)
                SetMatchWantSide
            ]
        -- Done button
        , H.div_ [ HP.class_ "flex gap-2 justify-end mt-3" ]
            [ H.button_
                [ HP.class_ "btn btn-outline btn-sm text-foreground"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick DismissReadyPopover
                ]
                [ text "Cancel" ]
            , H.button_
                [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick ConfirmMatchFilters
                ]
                [ text "Done" ]
            ]
        ]
    ]

-- | A labeled row of segmented toggle buttons.
filterSection
  :: MisoString
  -> [(MisoString, MisoString)]  -- (label, value) pairs
  -> MisoString                  -- current value
  -> (MisoString -> Action)      -- setter action
  -> View Model Action
filterSection label opts current setter =
  H.div_ [ HP.class_ "mb-2" ]
    [ H.div_ [ HP.class_ "text-xs text-muted-foreground mb-1" ] [ text label ]
    , H.div_ [ HP.class_ "flex rounded-md overflow-hidden border border-border" ]
        (map mkBtn opts)
    ]
  where
    mkBtn (lbl, val) =
      H.button_
        [ HP.class_ (if val == current
            then "flex-1 text-xs py-1 px-2 bg-muted text-foreground font-medium border-0 cursor-pointer"
            else "flex-1 text-xs py-1 px-2 bg-transparent text-muted-foreground border-0 cursor-pointer hover:bg-muted/50")
        , style_ [("touch-action", "manipulation")]
        , SVG.onClick (setter val)
        ]
        [ text lbl ]
