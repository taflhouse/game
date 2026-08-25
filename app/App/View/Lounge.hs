{-# LANGUAGE OverloadedStrings #-}
module App.View.Lounge (viewLounge) where

import Data.List (nub)
import Miso
import Miso.CSS (style_)
import Miso.String (MisoString, ms)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Supabase.Miso.Auth (Session(..), User(..))

import Tafl.Board (Piece(..), Board, Coords(..), boardSize, pieceAt)
import Tafl.Game (initialState, act)
import Tafl.Game.State (GameState(..))
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
    [ HP.class_ "w-full max-w-7xl" ]
    ( [ viewHero ]
   ++ (if mLoungeLoading m then []
        else
          (if null allGames
             then []
             else [ viewFilterPills (mLoungeFilter m) allGames ]
               ++ [ viewGameSection True  "LIVE GAMES" filteredLive viewLiveCard | not (null filteredLive) ]
               ++ [ viewGameSection False "OPEN GAMES" filteredOpen (viewOpenCard m) | not (null filteredOpen) ]
          )
        ++ lowerSection
       )
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
    hasContent   = not (null allGames) || not (null (mRankings m))
    myUsername   = fmap pUsername (mProfile m)
    lowerItems =
         [ viewRankings myUsername (mRankings m) | not (null (mRankings m)) ]
      ++ [ viewEpigraph | hasContent ]
    lowerSection = case lowerItems of
      []  -> []
      [x] -> [ x ]
      xs  -> [ H.div_ [ HP.class_ "lounge-lower-grid" ] xs ]

-- | Check if a game row belongs to the current user.
isOwnGame :: Maybe MisoString -> GameRow -> Bool
isOwnGame Nothing _ = False
isOwnGame (Just uid) gr =
  grwAttackerId gr == Just uid || grwDefenderId gr == Just uid

-- ---------------------------------------------------------------------------
-- Hero
-- ---------------------------------------------------------------------------

viewHero :: View Model Action
viewHero =
  H.div_
    [ HP.class_ "lounge-hero" ]
    [ H.div_
        [ HP.class_ "lounge-hero-copy" ]
        [ H.h1_
            [ HP.class_ "lounge-hero-title" ]
            [ text "One king."
            , H.br_ []
            , text "Sixteen attackers."
            , H.br_ []
            , H.em_ [] [ text "Four ways out." ]
            ]
        , H.p_
            [ HP.class_ "lounge-hero-lede" ]
            [ text "Play Hnefatafl and its regional variants against real opponents; practice against AI or pick up a live match." ]
        , H.div_
            [ HP.class_ "lounge-cta-card" ]
            [ H.button_
                [ HP.class_ "btn-lg w-full"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick GotoConfig
                ]
                [ iconPlus, text "New Game" ]
            , viewOrDivider
            , H.button_
                [ HP.class_ "btn-lg-ghost w-full"
                , style_ [("touch-action", "manipulation")]
                , SVG.onClick GotoJoin
                ]
                [ text "Join by Code" ]
            ]
        ]
    , viewBoardArt
    ]

iconPlus :: View Model Action
iconPlus =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24", HP.width_ "16", HP.height_ "16"
    , SP.fill_ "none", SP.stroke_ "currentcolor", SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M12 5v14M5 12h14" ] ]

-- | Ambient tablut board (9x9 opening position, per boardTablut in
-- src/Tafl/Game/Board.hs) shown beside the hero copy on wide screens.
-- Purely decorative — hidden below the hero's collapse breakpoint (see
-- .lounge-board-art in styles.css).
viewBoardArt :: View Model Action
viewBoardArt =
  H.div_
    [ HP.class_ "lounge-board-art" ]
    [ SVG.svg_
        [ SP.viewBox_ "0 0 220 220" ]
        [ SVG.g_ [ HP.class_ "lounge-board-grid" ] boardGridLines
        , SVG.rect_
            [ HP.class_ "lounge-board-edge"
            , SP.x_ "2", SP.y_ "2", HP.width_ "198", HP.height_ "198"
            ]
        , SVG.g_ [ HP.class_ "lounge-piece-attacker" ] (map dot attackerCoords)
        , SVG.g_ [ HP.class_ "lounge-piece-defender" ] (map dot defenderCoords)
        , SVG.circle_
            [ HP.class_ "lounge-king-ring", SP.cx_ "101", SP.cy_ "101", SP.r_ "9" ]
        , SVG.polygon_
            [ HP.class_ "lounge-piece-king"
            , SP.points_ "101,93.5 108.5,107 93.5,107"
            ]
        ]
    ]
  where
    dot (cx, cy) =
      SVG.circle_ [ SP.cx_ (ms (show (cx :: Int))), SP.cy_ (ms (show (cy :: Int))), SP.r_ "5" ]

    -- 9x9 grid, cell = 22 (edges at 2 and 200, same frame as before).
    gridCoords :: [Int]
    gridCoords = [2, 24 .. 200]

    boardGridLines :: [View Model Action]
    boardGridLines =
         [ SVG.line_ [ SP.x1_ (ms (show p)), SP.y1_ "2", SP.x2_ (ms (show p)), SP.y2_ "200" ] | p <- gridCoords ]
      ++ [ SVG.line_ [ SP.x1_ "2", SP.y1_ (ms (show p)), SP.x2_ "200", SP.y2_ (ms (show p)) ] | p <- gridCoords ]

    -- Attackers: four groups of four at the cardinal edges (16 total).
    attackerCoords :: [(Int, Int)]
    attackerCoords =
      [ (79,13),(101,13),(123,13),(101,35)
      , (79,189),(101,189),(123,189),(101,167)
      , (13,79),(13,101),(13,123),(35,101)
      , (189,79),(189,101),(189,123),(167,101)
      ]

    -- Defenders: the tight cross around the king (8 total).
    defenderCoords :: [(Int, Int)]
    defenderCoords =
      [ (101,57),(101,79)
      , (57,101),(79,101),(123,101),(145,101)
      , (101,123),(101,145)
      ]

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

viewGameSection :: Bool -> MisoString -> [GameRow] -> (GameRow -> View Model Action) -> View Model Action
viewGameSection isLive title games renderCard =
  H.div_
    [ HP.class_ "mb-6" ]
    [ H.div_
        [ HP.class_ "lounge-section-head" ]
        [ H.h2_
            []
            ( [ H.span_ [ HP.class_ "lounge-live-dot" ] [] | isLive ]
           ++ [ text (title <> " (" <> ms (show (length games)) <> ")") ]
            )
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
    [ HP.class_ "card lounge-game-card p-4 cursor-pointer"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (GotoReplay (grwId gr))
    ]
    ( [ H.div_
          [ HP.class_ "flex justify-between items-center gap-2" ]
          [ H.span_
              [ HP.class_ "font-medium text-foreground text-sm" ]
              [ text (playerNames gr) ]
          , H.span_
              [ HP.class_ "lounge-chip" ]
              [ text (variantLabel gr) ]
          ]
      ]
   ++ [ viewMiniBoard board | Just board <- [gameBoard gr] ]
   ++ [ H.div_
          [ HP.class_ "text-xs text-muted-foreground mt-1" ]
          [ text (turnLabel gr <> " \xB7 " <> ms (show (grwTotalMoves gr)) <> " moves" <> timeLabel gr) ]
      ]
    )

-- | Replay a game's real move list through the actual engine to get its
-- current board — the same reconstruction the game screen itself uses
-- (see CLAUDE.md: "Game state is reconstructed from the move list").
gameBoard :: GameRow -> Maybe Board
gameBoard gr = do
  variant <- lookupVariant (grwVariant gr)
  pure (gsBoard (foldl act (initialState variant) (grwMoves gr)))

-- | Small read-only board thumbnail for a live-game card.
viewMiniBoard :: Board -> View Model Action
viewMiniBoard board =
  H.div_
    [ HP.class_ "lounge-mini-board"
    , style_ [("grid-template-columns", "repeat(" <> ms (show n) <> ", 1fr)")]
    ]
    [ H.span_ [ HP.class_ (miniCellClass (pieceAt board (Coords r c))) ] []
    | r <- [0 .. n - 1]
    , c <- [0 .. n - 1]
    ]
  where
    n = boardSize board
    miniCellClass Empty    = "lounge-mini-cell"
    miniCellClass Attacker = "lounge-mini-cell lounge-mini-a"
    miniCellClass Defender = "lounge-mini-cell lounge-mini-d"
    miniCellClass King     = "lounge-mini-cell lounge-mini-k"

viewOpenCard :: Model -> GameRow -> View Model Action
viewOpenCard _m gr =
  H.div_
    [ HP.class_ "card lounge-game-card p-4 flex justify-between items-center" ]
    [ H.div_
        []
        [ H.div_
            [ HP.class_ "flex items-center gap-2" ]
            [ H.span_
                [ HP.class_ "font-medium text-foreground text-sm" ]
                [ text (creatorName gr) ]
            , H.span_
                [ HP.class_ "lounge-chip" ]
                [ text (variantLabel gr) ]
            ]
        , H.div_
            [ HP.class_ "text-xs text-muted-foreground mt-1" ]
            [ text ("Waiting" <> timeLabel gr) ]
        ]
    , case grwInviteCode gr of
        Just code ->
          H.button_
            [ HP.class_ "btn-sm-primary"
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

viewRankings :: Maybe MisoString -> [Profile] -> View Model Action
viewRankings myUsername profiles =
  H.div_
    [ HP.class_ "lounge-rankings" ]
    [ H.div_
        [ HP.class_ "lounge-rankings-head" ]
        [ H.h2_ [ HP.class_ "lounge-rankings-title" ] [ text "Ladder" ] ]
    , H.div_
        []
        (zipWith (viewRankingRow myUsername) [1..] profiles)
    ]

viewRankingRow :: Maybe MisoString -> Int -> Profile -> View Model Action
viewRankingRow myUsername rank p =
  H.div_
    [ HP.class_ (if myUsername == Just (pUsername p)
                   then "lounge-rank-row lounge-rank-row-me"
                   else "lounge-rank-row")
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick (GotoPlayer (pUsername p))
    ]
    [ H.span_ [ HP.class_ "lounge-rank-n" ] [ text (ms (show rank)) ]
    , H.span_ [ HP.class_ "lounge-rank-name" ] [ text (pUsername p) ]
    , H.span_ [ HP.class_ "lounge-rank-rating" ] [ text (ms (show (round (pRating p) :: Int))) ]
    , H.span_ [ HP.class_ "lounge-rank-games" ] [ text (ms (show (pGamesRated p)) <> " games") ]
    ]

-- ---------------------------------------------------------------------------
-- Epigraph
-- ---------------------------------------------------------------------------

viewEpigraph :: View Model Action
viewEpigraph =
  H.div_
    [ HP.class_ "lounge-epigraph" ]
    [ H.p_ [] [ text "\x201CThey played tafl in the meadow and were merry.\x201D" ]
    , H.cite_ [] [ text "\x2014 Vǫluspá, stanza 8" ]
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
