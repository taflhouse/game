{-# LANGUAGE OverloadedStrings #-}
module App.View.Config (viewConfig, viewConfigure) where

import Miso
import Miso.CSS (style_)
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import Tafl.Board (Side(..))
import Tafl.Rules (BoardVariant(..))

import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import App.JSON (Profile(..))
import App.Model
import App.Action

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
        , style_ [("margin-top", "4em"), ("gap", "0")]
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
            [ HP.class_ "mt-6 flex flex-col items-center gap-2"
            ]
            (if mGameMode m == MultiplayerMode
                then let nameNeeded = case mProfile m of
                           Just p | pUsername p /= "" -> False
                           _ -> mGuestName m == Nothing
                         disabled' = nameNeeded && mJoinNameInput m == ""
                     in [ H.button_
                            ([ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                             , style_ [("touch-action", "manipulation")]
                             , SVG.onClick FindMatch
                             ] ++ [ HP.disabled_ | disabled' ])
                            [ text "Find Match" ]
                        , H.div_
                            [ HP.class_ "flex items-center gap-2 w-full mt-1" ]
                            [ H.hr_ [ HP.class_ "flex-1 border-border" ]
                            , H.span_ [ HP.class_ "text-xs text-muted-foreground" ] [ text "or create a private game" ]
                            , H.hr_ [ HP.class_ "flex-1 border-border" ]
                            ]
                        , H.button_
                            ([ HP.class_ "btn w-full btn-outline text-foreground font-bold"
                             , style_ [("touch-action", "manipulation")]
                             , SVG.onClick CreateMultiplayerGame
                             ] ++ [ HP.disabled_ | disabled' ])
                            [ text "Create" ]
                        ]
                else [ H.button_
                         [ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                         , style_ [("touch-action", "manipulation")]
                         , SVG.onClick StartGame
                         ]
                         [ text "Start" ]
                     ]
            ++ [ H.span_
                   [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
                   , style_ [("touch-action", "manipulation")]
                   , SVG.onClick GotoConfig
                   ]
                   [ text "Back" ]
               ]
            )
        ]
    ]

-- ---------------------------------------------------------------------------
-- Setup helpers
-- ---------------------------------------------------------------------------

setupSection :: MisoString -> [View Model Action] -> View Model Action
setupSection label children =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-6"
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

disabledBtn :: MisoString -> View Model Action
disabledBtn label =
  H.button_
    [ HP.class_ "btn btn-outline text-muted-foreground"
    , style_ [("touch-action", "manipulation"), ("opacity", "0.5"), ("cursor", "not-allowed")]
    , HP.disabled_
    ]
    [ text label ]

viewSetupAi :: Model -> View Model Action
viewSetupAi m =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-6"
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
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-6 flex items-center justify-center gap-1" ]
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
            [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-6 flex items-center justify-center gap-1" ]
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
  let needsName = case mProfile m of
        Just p | pUsername p /= "" -> False
        _ -> case mSession m of
          Nothing -> True                       -- no session yet
          Just _  -> mGuestName m /= Nothing    -- anonymous user
      isAnon = case mSession m of
        Just sess -> amProvider (userAppMetadata (sessionUser sess)) == "anonymous"
        Nothing   -> True
  in H.div_
    [ HP.class_ "text-center" ]
    ([ setupSection "Your Side"
        [ setupBtn (SetSidePreference "attacker") "Attackers" (mSidePreference m == "attacker")
        , setupBtn (SetSidePreference "defender") "Defenders" (mSidePreference m == "defender")
        , setupBtn (SetSidePreference "either") "Either" (mSidePreference m == "either")
        ]
    , setupSection "Game Type"
        [ if isAnon
            then disabledBtn "Rated"
            else setupBtn (SetRated True) "Rated" (mIsRated m)
        , setupBtn (SetRated False) "Casual" (isAnon || not (mIsRated m))
        ]
    ] ++ [ H.p_
             [ HP.class_ "text-xs text-muted-foreground" ]
             [ text "Sign in to play rated games" ]
         | isAnon ]
    ++ [ setupSection "Time Control"
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
            , setupBtn (SetTimeControl (BlitzControl 1200000)) "20 min" (mTimeControl m == BlitzControl 1200000)
            ]
        DailyControl _ ->
          setupSection "Time Per Move"
            [ setupBtn (SetTimeControl (DailyControl 86400)) "1 day" (mTimeControl m == DailyControl 86400)
            , setupBtn (SetTimeControl (DailyControl 172800)) "2 days" (mTimeControl m == DailyControl 172800)
            , setupBtn (SetTimeControl (DailyControl 259200)) "3 days" (mTimeControl m == DailyControl 259200)
            ]
        NoTimeControl -> text ""
    ] ++ [ setupSection "Your Name"
             [ H.input_
                 [ HP.class_ "input w-full text-center"
                 , HP.type_ "text"
                 , HP.placeholder_ "Enter your name"
                 , HP.value_ (if mJoinNameInput m /= "" then mJoinNameInput m
                              else maybe "" id (mGuestName m))
                 , H.onInput SetJoinNameInput
                 ]
             ]
         | needsName ]
    )

isBlitz :: TimeControl -> Bool
isBlitz (BlitzControl _) = True
isBlitz _                = False

isDaily :: TimeControl -> Bool
isDaily (DailyControl _) = True
isDaily _                = False
