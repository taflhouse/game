{-# LANGUAGE OverloadedStrings #-}
module App.View.Lounge (viewLounge) where

import Data.List (nub)
import Miso
import Miso.CSS (style_)
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG

import Supabase.Miso.Auth (Session(..), User(..))

import Tafl.Rules (BoardVariant(..))

import App.JSON (GameRow(..), Profile(..))
import App.Model
import App.Action
import App.Route (variantSlugMs, variantName, lookupVariant)

-- ---------------------------------------------------------------------------
-- Lounge Screen
-- ---------------------------------------------------------------------------

viewLounge :: Model -> View Model Action
viewLounge m =
  H.div_
    [ HP.class_ "w-full max-w-2xl"
    , style_ [("margin-top", "3em")]
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
              [ text "Join by Code" ]
          ]
      ]
    ++ (if mLoungeLoading m && null allGames
        then [ H.div_
                 [ HP.class_ "text-sm text-muted-foreground animate-pulse text-center py-8" ]
                 [ text "Loading..." ]
             ]
        else if null allGames
          then []
          else [ viewFilterPills (mLoungeFilter m) allGames ]
            ++ [ viewGameSection "LIVE GAMES" filteredLive viewLiveCard | not (null filteredLive) ]
            ++ [ viewGameSection "OPEN GAMES" filteredOpen (viewOpenCard m) | not (null filteredOpen) ]
       )
    ++ [ viewRankings (mRankings m) | not (null (mRankings m)) ]
    ++ [ H.div_
           [ HP.class_ ("text-center" <> if null allGames && null (mRankings m) && not (mLoungeLoading m) then " mt-4" else " mt-8") ]
           [ H.p_
               [ HP.class_ "text-muted-foreground text-sm italic" ]
               [ text "\x201CThey played tafl in the meadow and were merry\x201D" ]
           ]
       ]
    )
  where
    mUid = fmap (userId . sessionUser) (mSession m)
    -- Filter out the current user's own waiting games
    openGames = filter (not . isOwnGame mUid) (mLoungeOpen m)
    liveGames = mLoungeLive m
    allGames  = openGames ++ liveGames
    -- Apply client-side variant filter
    variantMatch gr = case mLoungeFilter m of
      Nothing -> True
      Just v  -> grwVariant gr == v
    filteredOpen = filter variantMatch openGames
    filteredLive = filter variantMatch liveGames

-- | Check if a game row belongs to the current user.
isOwnGame :: Maybe MisoString -> GameRow -> Bool
isOwnGame Nothing _ = False
isOwnGame (Just uid) gr =
  grwAttackerId gr == Just uid || grwDefenderId gr == Just uid

-- ---------------------------------------------------------------------------
-- Filter pills
-- ---------------------------------------------------------------------------

viewFilterPills :: Maybe MisoString -> [GameRow] -> View Model Action
viewFilterPills activeFilter allGames =
  let presentVariants = nub [ v | gr <- allGames, Just v <- [lookupVariant (grwVariant gr)] ]
      pills = case presentVariants of
        []  -> []  -- no games at all, no pills
        [_] -> []  -- only one variant, filtering is pointless
        _   -> pill "All" Nothing : map variantPill presentVariants
  in H.div_
    [ HP.class_ "flex flex-wrap gap-2 justify-center mb-6" ]
    pills
  where
    pill label mSlug =
      let active = activeFilter == mSlug
      in H.button_
        [ HP.class_ (if active
            then "px-3 py-1 text-xs rounded-full bg-primary text-primary-foreground cursor-pointer border-0"
            else "px-3 py-1 text-xs rounded-full bg-muted text-muted-foreground hover:bg-muted/80 cursor-pointer border-0")
        , style_ [("touch-action", "manipulation")]
        , SVG.onClick (SetLoungeFilter mSlug)
        ]
        [ text label ]

    variantPill v = pill (variantName v) (Just (variantSlugMs v))

-- ---------------------------------------------------------------------------
-- Game sections
-- ---------------------------------------------------------------------------

viewGameSection :: MisoString -> [GameRow] -> (GameRow -> View Model Action) -> View Model Action
viewGameSection title games renderCard =
  H.div_
    [ HP.class_ "mb-6" ]
    [ H.div_
        [ HP.class_ "flex items-center gap-2 mb-3" ]
        [ H.span_
            [ HP.class_ "text-xs font-semibold text-muted-foreground uppercase tracking-wider" ]
            [ text (title <> " (" <> ms (show (length games)) <> ")") ]
        ]
    , H.div_
        [ HP.class_ "flex flex-col gap-2" ]
        (map renderCard games)
    ]

-- ---------------------------------------------------------------------------
-- Game cards
-- ---------------------------------------------------------------------------

viewLiveCard :: GameRow -> View Model Action
viewLiveCard gr =
  H.div_
    [ HP.class_ "card p-4 cursor-pointer hover:bg-muted/50"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (GotoReplay (grwId gr))
    ]
    [ H.div_
        [ HP.class_ "flex justify-between items-center" ]
        [ H.span_
            [ HP.class_ "font-medium text-foreground" ]
            [ text (variantLabel gr) ]
        , H.span_
            [ HP.class_ "text-sm text-muted-foreground" ]
            [ text (playerNames gr) ]
        ]
    , H.div_
        [ HP.class_ "text-xs text-muted-foreground mt-1" ]
        [ text (turnLabel gr <> " \xB7 " <> ms (show (grwTotalMoves gr)) <> " moves" <> timeLabel gr) ]
    ]

viewOpenCard :: Model -> GameRow -> View Model Action
viewOpenCard _m gr =
  H.div_
    [ HP.class_ "card p-4 flex justify-between items-center" ]
    [ H.div_
        []
        [ H.div_
            [ HP.class_ "flex items-center gap-2" ]
            [ H.span_
                [ HP.class_ "font-medium text-foreground" ]
                [ text (variantLabel gr) ]
            , H.span_
                [ HP.class_ "text-sm text-muted-foreground" ]
                [ text (creatorName gr) ]
            ]
        , H.div_
            [ HP.class_ "text-xs text-muted-foreground mt-1" ]
            [ text ("Waiting" <> timeLabel gr) ]
        ]
    , case grwInviteCode gr of
        Just code ->
          H.button_
            [ HP.class_ "btn text-sm"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick (JoinFromLounge code)
            ]
            [ text "Join" ]
        Nothing -> text ""
    ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

variantLabel :: GameRow -> MisoString
variantLabel gr = case lookupVariantName (grwVariant gr) of
  Just name -> name
  Nothing   -> grwVariant gr

lookupVariantName :: MisoString -> Maybe MisoString
lookupVariantName slug = lookup slug
  [ (variantSlugMs v, variantName v) | v <- [minBound .. maxBound] ]

playerNames :: GameRow -> MisoString
playerNames gr =
  let aN = maybe "?" id (grwAttackerName gr)
      dN = maybe "?" id (grwDefenderName gr)
  in aN <> " vs " <> dN

creatorName :: GameRow -> MisoString
creatorName gr = case grwAttackerName gr of
  Just name -> name <> " (Attackers)"
  Nothing -> case grwDefenderName gr of
    Just name -> name <> " (Defenders)"
    Nothing   -> "Anonymous"

turnLabel :: GameRow -> MisoString
turnLabel gr = case grwCurrentTurn gr of
  "attacker" -> "Attacker's turn"
  "defender" -> "Defender's turn"
  _          -> "In progress"

timeLabel :: GameRow -> MisoString
timeLabel gr = case grwTimeControl gr of
  Just "blitz" -> case grwTimePerPlayerMs gr of
    Just ms' -> " \xB7 " <> formatTimeMs ms'
    Nothing  -> ""
  Just "daily" -> case grwTimePerMoveSec gr of
    Just s   -> " \xB7 " <> ms (show (s `div` 60)) <> " min/move"
    Nothing  -> ""
  _            -> ""

formatTimeMs :: Int -> MisoString
formatTimeMs totalMs =
  let mins = totalMs `div` 60000
  in ms (show mins) <> " min"

-- ---------------------------------------------------------------------------
-- Rankings
-- ---------------------------------------------------------------------------

viewRankings :: [Profile] -> View Model Action
viewRankings profiles =
  H.div_
    [ HP.class_ "mb-6" ]
    [ H.div_
        [ HP.class_ "flex items-center gap-2 mb-3" ]
        [ H.span_
            [ HP.class_ "text-xs font-semibold text-muted-foreground uppercase tracking-wider" ]
            [ text "RANKINGS" ]
        ]
    , H.div_
        [ HP.class_ "overflow-x-auto" ]
        [ H.table_
            [ HP.class_ "table w-full" ]
            [ H.thead_
                []
                [ H.tr_
                    []
                    [ H.th_ [] [ text "#" ]
                    , H.th_ [] [ text "Player" ]
                    , H.th_ [] [ text "Rating" ]
                    , H.th_ [] [ text "Games" ]
                    ]
                ]
            , H.tbody_
                []
                (zipWith viewRankingRow [1..] profiles)
            ]
        ]
    ]

viewRankingRow :: Int -> Profile -> View Model Action
viewRankingRow rank p =
  H.tr_
    [ HP.class_ "cursor-pointer hover:bg-muted/50"
    , SVG.onClick (GotoPlayer (pUsername p))
    ]
    [ H.td_ [] [ text (ms (show rank)) ]
    , H.td_ [ HP.class_ "font-medium" ] [ text (pUsername p) ]
    , H.td_ [] [ text (ms (show (round (pRating p) :: Int))) ]
    , H.td_ [] [ text (ms (show (pGamesRated p))) ]
    ]

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
