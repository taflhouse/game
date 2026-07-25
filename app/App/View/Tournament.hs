{-# LANGUAGE OverloadedStrings #-}
module App.View.Tournament
  ( viewTournaments
  , viewTournamentDetail
  , viewCreateTournament
  ) where

import Data.List (sortOn)
import Data.Maybe (isJust, isNothing)
import Miso
import Miso.CSS (style_)
import Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import Tafl.Board (Side(..))
import Tafl.Rules (BoardVariant(..))

import Supabase.Miso.Auth (Session(..), User(..))

import App.JSON (Profile(..), TournamentRow(..), TournamentPlayerRow(..), TournamentPairingRow(..))
import App.Model
import App.Action
import App.Route (variantName, lookupVariant, playURI, gamePermalinkURI)

-- ---------------------------------------------------------------------------
-- Tournament List (/tournaments)
-- ---------------------------------------------------------------------------

viewTournaments :: Model -> View Model Action
viewTournaments m =
  H.div_
    [ HP.class_ "w-full max-w-2xl"
    , style_ [("margin-top", "3em")]
    ]
    [ H.div_
        [ HP.class_ "flex items-center justify-between mb-6" ]
        [ H.h1_ [ HP.class_ "text-xl font-bold" ] [ text "Tournaments" ]
        , case mSession m of
            Just _ | isJust (mProfile m) ->
              H.button_
                [ HP.class_ "btn bg-green-600 hover:bg-green-700 text-white border-green-500"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick GotoCreateTournament
                ]
                [ text "New Tournament" ]
            _ -> text ""
        ]
    -- Join by code
    , H.div_
        [ HP.class_ "flex gap-2 mb-6" ]
        [ H.input_
            [ HP.class_ "input flex-1"
            , HP.type_ "text"
            , HP.placeholder_ "Enter invite code"
            , HP.value_ (mTournamentCodeInput m)
            , H.onInput SetTournamentCodeInput
            ]
        , H.button_
            [ HP.class_ "btn btn-outline text-foreground"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick LookupTournamentByCode
            ]
            [ text "Join" ]
        ]
    , if mTournamentsLoading m
        then H.div_
          [ HP.class_ "text-center text-muted-foreground animate-pulse"
          , style_ [("margin-top", "4em")]
          ]
          [ text "Loading..." ]
        else if null (mTournaments m)
          then H.div_
            [ HP.class_ "text-center text-muted-foreground"
            , style_ [("margin-top", "4em")]
            ]
            [ text "No tournaments yet. Create one to get started!" ]
          else viewTournamentsList (mTournaments m)
    ]

viewTournamentsList :: [TournamentRow] -> View Model Action
viewTournamentsList ts =
  let registration = filter (\t -> trStatus t == "registration") ts
      active       = filter (\t -> trStatus t == "active") ts
      finished     = filter (\t -> trStatus t == "finished") ts
  in H.div_ [ HP.class_ "flex flex-col gap-6" ]
    (  [ viewTournamentSection "REGISTRATION OPEN" registration | not (null registration) ]
    ++ [ viewTournamentSection "ACTIVE" active | not (null active) ]
    ++ [ viewTournamentSection "FINISHED" finished | not (null finished) ]
    )

viewTournamentSection :: MisoString -> [TournamentRow] -> View Model Action
viewTournamentSection title ts =
  H.div_
    [ HP.class_ "mb-2" ]
    [ H.div_
        [ HP.class_ "flex items-center gap-2 mb-3" ]
        [ H.span_
            [ HP.class_ "text-xs font-semibold text-muted-foreground uppercase tracking-wider" ]
            [ text (title <> " (" <> ms (show (length ts)) <> ")") ]
        ]
    , H.div_
        [ HP.class_ "flex flex-col gap-2" ]
        (map viewTournamentCard ts)
    ]

viewTournamentCard :: TournamentRow -> View Model Action
viewTournamentCard t =
  H.div_
    [ HP.class_ "card p-4 cursor-pointer hover:bg-muted/50"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (GotoTournament (trId t))
    ]
    [ H.div_
        [ HP.class_ "flex justify-between items-center" ]
        [ H.span_ [ HP.class_ "font-medium text-foreground" ] [ text (trName t) ]
        , statusBadge (trStatus t)
        ]
    , H.div_
        [ HP.class_ "flex gap-2 mt-2 flex-wrap" ]
        [ formatBadge (trFormat t)
        , H.span_ [ HP.class_ "text-xs text-muted-foreground" ]
            [ text (variantLabel (trVariant t)) ]
        , H.span_ [ HP.class_ "text-xs text-muted-foreground" ]
            [ text ("by " <> trOrganizerName t) ]
        ]
    , H.div_
        [ HP.class_ "text-xs text-muted-foreground mt-1" ]
        [ text (case trStatus t of
            "active" -> "Round " <> ms (show (trCurrentRound t))
                     <> maybe "" (\n -> " / " <> ms (show n)) (trTotalRounds t)
            _        -> if trIsRated t then "Rated" else "Casual")
        ]
    ]

-- ---------------------------------------------------------------------------
-- Tournament Detail (/tournaments/:id)
-- ---------------------------------------------------------------------------

viewTournamentDetail :: Model -> View Model Action
viewTournamentDetail m = case mTournament m of
  Nothing | mTournamentLoading m ->
    H.div_
      [ HP.class_ "text-center text-muted-foreground animate-pulse"
      , style_ [("margin-top", "4em")]
      ]
      [ text "Loading..." ]
  Nothing ->
    H.div_
      [ HP.class_ "text-center text-muted-foreground"
      , style_ [("margin-top", "4em")]
      ]
      [ text "Tournament not found." ]
  Just t ->
    let players = mTournamentPlayers m
        pairings = mTournamentPairings m
        mUid = fmap (userId . sessionUser) (mSession m)
        isOrganizer = fmap (== trOrganizerId t) mUid == Just True
        isJoined = any (\p -> Just (tpPlayerId p) == mUid) players
        canJoin = trStatus t == "registration"
                  && isJust (mSession m) && isJust (mProfile m)
                  && not isJoined
                  && maybe True (\mx -> length players < mx) (trMaxPlayers t)
        canStart = isOrganizer && trStatus t == "registration" && length players >= 2
        roundN = mTournamentRound m
        roundPairings = filter (\p -> tprRoundNumber p == roundN) pairings
    in H.div_
      [ HP.class_ "w-full max-w-2xl"
      , style_ [("margin-top", "3em")]
      ]
      [ -- Header
        H.div_ [ HP.class_ "mb-6" ]
          [ H.div_ [ HP.class_ "flex items-center justify-between mb-2" ]
              [ H.h1_ [ HP.class_ "text-xl font-bold" ] [ text (trName t) ]
              , statusBadge (trStatus t)
              ]
          , H.div_ [ HP.class_ "flex gap-2 flex-wrap text-sm text-muted-foreground" ]
              [ formatBadge (trFormat t)
              , H.span_ [] [ text (variantLabel (trVariant t)) ]
              , H.span_ [] [ text (if trIsRated t then "Rated" else "Casual") ]
              , case trTimeControl t of
                  Just "blitz" -> H.span_ [] [ text (maybe "Blitz" (\ms' -> ms (show (ms' `div` 60000)) <> " min") (trTimePerPlayerMs t)) ]
                  Just "daily" -> H.span_ [] [ text (maybe "Daily" (\s -> ms (show (s `div` 3600)) <> " hr/move") (trTimePerMoveSec t)) ]
                  _            -> text ""
              , case trStatus t of
                  "active" -> H.span_ [] [ text ("Round " <> ms (show (trCurrentRound t)) <> maybe "" (\n -> " / " <> ms (show n)) (trTotalRounds t)) ]
                  _        -> text ""
              ]
          , case trDescription t of
              Just desc | desc /= "" -> H.p_ [ HP.class_ "text-sm text-muted-foreground mt-2" ] [ text desc ]
              _ -> text ""
          ]
      -- Action buttons
      , H.div_ [ HP.class_ "flex gap-2 mb-6 flex-wrap" ]
          [ if canJoin
              then H.button_
                [ HP.class_ "btn bg-green-600 hover:bg-green-700 text-white border-green-500"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick (JoinTournament (trId t))
                ]
                [ text "Join Tournament" ]
              else text ""
          , if canStart
              then H.button_
                [ HP.class_ "btn bg-green-600 hover:bg-green-700 text-white border-green-500"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick (StartTournament (trId t))
                ]
                [ text "Start Tournament" ]
              else text ""
          , if trIsPrivate t
              then case trInviteCode t of
                Just code -> H.div_ [ HP.class_ "text-sm text-muted-foreground" ]
                  [ text "Invite code: "
                  , H.span_ [ HP.class_ "font-mono font-bold select-all" ] [ text code ]
                  ]
                Nothing -> text ""
              else text ""
          ]
      -- Standings
      , if null players
          then text ""
          else viewStandings mUid (trStatus t) players
      -- Round tabs + pairings
      , if trCurrentRound t > 0
          then H.div_ [ HP.class_ "mt-6" ]
              [ H.div_ [ HP.class_ "flex gap-1 mb-4 flex-wrap" ]
                  [ roundTab n roundN | n <- [1 .. trCurrentRound t] ]
              , if null roundPairings
                  then H.div_ [ HP.class_ "text-sm text-muted-foreground text-center" ]
                      [ text "No pairings for this round." ]
                  else H.div_ [ HP.class_ "flex flex-col gap-2" ]
                      (map viewPairingCard roundPairings)
              ]
          else text ""
      -- Back link
      , H.div_ [ HP.class_ "mt-8 text-center" ]
          [ H.span_
              [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GotoTournaments
              ]
              [ text "Back to Tournaments" ]
          ]
      ]

viewStandings :: Maybe MisoString -> MisoString -> [TournamentPlayerRow] -> View Model Action
viewStandings mUid status players =
  let sorted = sortOn (\p -> (negate (tpScore p), negate (tpBuchholz p))) players
      showStats = status /= "registration"
  in H.div_ [ HP.class_ "mb-4" ]
    [ H.div_ [ HP.class_ "flex items-center gap-2 mb-3" ]
        [ H.span_ [ HP.class_ "text-xs font-semibold text-muted-foreground uppercase tracking-wider" ]
            [ text ("PLAYERS (" <> ms (show (length players)) <> ")") ]
        ]
    , H.div_ [ HP.class_ "overflow-x-auto" ]
        [ H.table_ [ HP.class_ "table w-full" ]
            [ H.thead_ []
                [ H.tr_ []
                    ([ H.th_ [] [ text "#" ]
                     , H.th_ [] [ text "Player" ]
                     ] ++ if showStats
                       then [ H.th_ [] [ text "Score" ]
                            , H.th_ [] [ text "W-L-D" ]
                            , H.th_ [] [ text "Buchholz" ]
                            , H.th_ [ HP.class_ "hidden sm:table-cell" ] [ text "Att W-L-D" ]
                            , H.th_ [ HP.class_ "hidden sm:table-cell" ] [ text "Def W-L-D" ]
                            ]
                       else [])
                ]
            , H.tbody_ []
                (zipWith (viewStandingRow mUid showStats) [1..] sorted)
            ]
        ]
    ]

viewStandingRow :: Maybe MisoString -> Bool -> Int -> TournamentPlayerRow -> View Model Action
viewStandingRow mUid showStats rank p =
  let isMe = mUid == Just (tpPlayerId p)
  in H.tr_
    [ HP.class_ (if isMe then "bg-muted/50" else "") ]
    ([ H.td_ [] [ text (ms (show rank)) ]
     , H.td_ [ HP.class_ "font-medium" ] [ text (tpPlayerName p) ]
     ] ++ if showStats
       then [ H.td_ [] [ text (ms (showScore (tpScore p))) ]
            , H.td_ [] [ text (ms (show (tpWins p)) <> "-" <> ms (show (tpLosses p)) <> "-" <> ms (show (tpDraws p))) ]
            , H.td_ [] [ text (ms (showScore (tpBuchholz p))) ]
            , H.td_ [ HP.class_ "hidden sm:table-cell" ]
                [ text (ms (show (tpAttackerWins p)) <> "-" <> ms (show (tpAttackerLosses p)) <> "-" <> ms (show (tpAttackerDraws p))) ]
            , H.td_ [ HP.class_ "hidden sm:table-cell" ]
                [ text (ms (show (tpDefenderWins p)) <> "-" <> ms (show (tpDefenderLosses p)) <> "-" <> ms (show (tpDefenderDraws p))) ]
            ]
       else [])

showScore :: Double -> String
showScore d =
  let whole = floor d :: Int
      half  = d - fromIntegral whole
  in if half >= 0.4 && half <= 0.6
     then show whole ++ ".5"
     else show (round d :: Int)

roundTab :: Int -> Int -> View Model Action
roundTab n current =
  H.button_
    [ HP.class_ (if n == current
        then "px-3 py-1 text-xs rounded-full bg-primary text-primary-foreground cursor-pointer border-0"
        else "px-3 py-1 text-xs rounded-full bg-muted text-muted-foreground hover:bg-muted/80 cursor-pointer border-0")
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (SetTournamentRound n)
    ]
    [ text ("Round " <> ms (show n)) ]

viewPairingCard :: TournamentPairingRow -> View Model Action
viewPairingCard p =
  let p1Side = tprPlayer1Side p
      p2Side = if p1Side == "attacker" then "defender" else "attacker"
      resultText = case tprResult p of
        Just "player_1" -> "Player 1 wins"
        Just "player_2" -> "Player 2 wins"
        Just "draw"     -> "Draw"
        Just "bye"      -> "Bye"
        Just "forfeit_1" -> "Player 1 forfeited"
        Just "forfeit_2" -> "Player 2 forfeited"
        _               -> "In progress"
      isBye = isNothing (tprPlayer2Id p)
  in H.div_
    [ HP.class_ "card p-3" ]
    [ H.div_ [ HP.class_ "flex justify-between items-center" ]
        [ H.div_ [ HP.class_ "flex flex-col gap-1" ]
            [ H.div_ [ HP.class_ "flex items-center gap-2" ]
                [ H.span_ [ HP.class_ "text-xs text-muted-foreground uppercase" ] [ text p1Side ]
                , H.span_ [ HP.class_ "font-medium text-sm" ] [ text (tprPlayer1Id p) ]
                ]
            , if isBye
                then H.div_ [ HP.class_ "text-sm text-muted-foreground italic" ] [ text "Bye" ]
                else H.div_ [ HP.class_ "flex items-center gap-2" ]
                  [ H.span_ [ HP.class_ "text-xs text-muted-foreground uppercase" ] [ text p2Side ]
                  , H.span_ [ HP.class_ "font-medium text-sm" ] [ text (maybe "?" id (tprPlayer2Id p)) ]
                  ]
            ]
        , H.div_ [ HP.class_ "text-right" ]
            [ H.div_ [ HP.class_ "text-xs text-muted-foreground" ] [ text resultText ]
            , case tprGameId p of
                Just gid | isJust (tprResult p) ->
                  H.span_
                    [ HP.class_ "text-xs text-muted-foreground hover:text-foreground cursor-pointer underline"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick (GotoReplay gid)
                    ]
                    [ text "Replay" ]
                Just gid ->
                  H.span_
                    [ HP.class_ "text-xs text-muted-foreground hover:text-foreground cursor-pointer underline"
                    , style_ [("touch-action", "manipulation")]
                    , SVG.onClick (GotoPlay gid)
                    ]
                    [ text "Watch" ]
                Nothing -> text ""
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Create Tournament (/tournaments/new)
-- ---------------------------------------------------------------------------

viewCreateTournament :: Model -> View Model Action
viewCreateTournament m =
  H.div_
    [ HP.class_ "w-full flex flex-col items-center" ]
    [ H.div_
        [ HP.class_ "card p-6 w-full max-w-md"
        , style_ [("margin-top", "4em"), ("gap", "0")]
        ]
        [ H.h2_ [ HP.class_ "text-xl font-bold mb-4 text-center" ]
            [ text "New Tournament" ]
        -- Name
        , formSection "Name"
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "text"
                , HP.placeholder_ "Tournament name"
                , HP.value_ (mTFormName m)
                , H.onInput SetTFormName
                ]
            ]
        -- Description
        , formSection "Description"
            [ H.input_
                [ HP.class_ "input w-full"
                , HP.type_ "text"
                , HP.placeholder_ "Optional description"
                , HP.value_ (mTFormDescription m)
                , H.onInput SetTFormDescription
                ]
            ]
        -- Format
        , formSection "Format"
            [ formBtn (SetTFormFormat "swiss") "Swiss" (mTFormFormat m == "swiss")
            , formBtn (SetTFormFormat "round_robin") "Round Robin" (mTFormFormat m == "round_robin")
            , formBtn (SetTFormFormat "single_elimination") "Elimination" (mTFormFormat m == "single_elimination")
            ]
        -- Variant
        , formSection "Board"
            [ formBtn (SetTFormVariant Brandubh) "Brandubh 7x7" (mTFormVariant m == Brandubh)
            , formBtn (SetTFormVariant Tablut) "Tablut 9x9" (mTFormVariant m == Tablut)
            , formBtn (SetTFormVariant Classic) "Copenhagen 11x11" (mTFormVariant m == Classic)
            , formBtn (SetTFormVariant Parlett) "Parlett 13x13" (mTFormVariant m == Parlett)
            , formBtn (SetTFormVariant DamienWalker) "Damien Walker 15x15" (mTFormVariant m == DamienWalker)
            ]
        -- Rated / Casual
        , formSection "Game Type"
            [ formBtn (SetTFormIsRated True) "Rated" (mTFormIsRated m)
            , formBtn (SetTFormIsRated False) "Casual" (not (mTFormIsRated m))
            ]
        -- Time control
        , formSection "Time Control"
            [ formBtn (SetTFormTimeControl NoTimeControl) "None" (mTFormTimeControl m == NoTimeControl)
            , formBtn (SetTFormTimeControl (BlitzControl 300000)) "Blitz" (isBlitz' (mTFormTimeControl m))
            , formBtn (SetTFormTimeControl (DailyControl 86400)) "Daily" (isDaily' (mTFormTimeControl m))
            ]
        , case mTFormTimeControl m of
            BlitzControl _ ->
              formSection "Time Per Player"
                [ formBtn (SetTFormTimeControl (BlitzControl 120000)) "2 min" (mTFormTimeControl m == BlitzControl 120000)
                , formBtn (SetTFormTimeControl (BlitzControl 300000)) "5 min" (mTFormTimeControl m == BlitzControl 300000)
                , formBtn (SetTFormTimeControl (BlitzControl 600000)) "10 min" (mTFormTimeControl m == BlitzControl 600000)
                , formBtn (SetTFormTimeControl (BlitzControl 1200000)) "20 min" (mTFormTimeControl m == BlitzControl 1200000)
                ]
            DailyControl _ ->
              formSection "Time Per Move"
                [ formBtn (SetTFormTimeControl (DailyControl 86400)) "1 day" (mTFormTimeControl m == DailyControl 86400)
                , formBtn (SetTFormTimeControl (DailyControl 172800)) "2 days" (mTFormTimeControl m == DailyControl 172800)
                , formBtn (SetTFormTimeControl (DailyControl 259200)) "3 days" (mTFormTimeControl m == DailyControl 259200)
                ]
            NoTimeControl -> text ""
        -- Max players
        , formSection "Max Players"
            [ H.input_
                [ HP.class_ "input w-full text-center"
                , HP.type_ "number"
                , HP.placeholder_ "Unlimited"
                , HP.value_ (mTFormMaxPlayers m)
                , H.onInput SetTFormMaxPlayers
                ]
            ]
        -- Private toggle
        , formSection "Visibility"
            [ formBtn (SetTFormIsPrivate False) "Public" (not (mTFormIsPrivate m))
            , formBtn (SetTFormIsPrivate True) "Private" (mTFormIsPrivate m)
            ]
        -- Create button
        , H.div_ [ HP.class_ "mt-6 flex flex-col items-center gap-2" ]
            [ H.button_
                ([ HP.class_ "btn w-full bg-green-600 hover:bg-green-700 text-white border-green-500 font-bold"
                 , style_ [("touch-action", "manipulation")]
                 , SVG.onClick CreateTournament
                 ] ++ [ HP.disabled_ | mTFormName m == "" ])
                [ text "Create Tournament" ]
            , H.span_
                [ HP.class_ "text-sm text-muted-foreground hover:text-foreground cursor-pointer"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick GotoTournaments
                ]
                [ text "Back" ]
            ]
        ]
    ]

-- ---------------------------------------------------------------------------
-- Form helpers
-- ---------------------------------------------------------------------------

formSection :: MisoString -> [View Model Action] -> View Model Action
formSection label children =
  H.div_
    [ HP.class_ "text-center" ]
    [ H.div_
        [ HP.class_ "text-muted-foreground text-xs tracking-[3px] uppercase mb-2 mt-6" ]
        [ text label ]
    , H.div_
        [ HP.class_ "flex gap-2 flex-wrap justify-center" ]
        children
    ]

formBtn :: Action -> MisoString -> Bool -> View Model Action
formBtn action label isActive =
  H.button_
    [ HP.class_ (if isActive then "btn btn-secondary" else "btn btn-outline text-foreground")
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

statusBadge :: MisoString -> View Model Action
statusBadge status =
  let (cls, label) = case status of
        "registration" -> ("bg-blue-500/20 text-blue-400", "Registration")
        "active"       -> ("bg-green-500/20 text-green-400", "Active")
        "finished"     -> ("bg-muted text-muted-foreground", "Finished")
        "cancelled"    -> ("bg-red-500/20 text-red-400", "Cancelled")
        _              -> ("bg-muted text-muted-foreground", status)
  in H.span_ [ HP.class_ ("px-2 py-0.5 text-xs rounded-full " <> cls) ] [ text label ]

formatBadge :: MisoString -> View Model Action
formatBadge fmt =
  let label = case fmt of
        "swiss"              -> "Swiss"
        "round_robin"        -> "Round Robin"
        "single_elimination" -> "Elimination"
        _                    -> fmt
  in H.span_ [ HP.class_ "px-2 py-0.5 text-xs rounded-full bg-muted text-muted-foreground" ] [ text label ]

variantLabel :: MisoString -> MisoString
variantLabel slug = case lookupVariant slug of
  Just v  -> variantName v
  Nothing -> slug

isBlitz' :: TimeControl -> Bool
isBlitz' (BlitzControl _) = True
isBlitz' _                = False

isDaily' :: TimeControl -> Bool
isDaily' (DailyControl _) = True
isDaily' _                = False
