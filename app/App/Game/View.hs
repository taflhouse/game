{-# LANGUAGE OverloadedStrings #-}
module App.Game.View (viewGame) where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Miso hiding ((!!))
import Miso.CSS (style_)
import Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP

import Tafl.Board
import Tafl.Game.State

import Supabase.Miso.Auth (Session(..), User(..), AppMetadata(..))

import App.JSON (Profile(..), ChatMessage(..))
import App.Model (GameMode(..), TimeControl(..), ViewMode(..))
import App.Board (sqSize, coordStr, svgDefs, renderSquareBg, renderSpecialSquares,
                  renderPiece, renderLastMove, viewBoardContainer, viewEvalBar, viewClocks)
import App.Game.Model
import App.Game.Action

-- | Main game view
viewGame :: GameProps -> GameModel -> View GameModel GameAction
viewGame props gm
  -- GameMount hasn't processed yet; render nothing to avoid a flash of
  -- the default board before the real game state is applied.
  | isNothing (gmGameId gm) = text ""
  | gmGameMode gm == MultiplayerMode, Just _ <- gmPlayerSide gm, Nothing <- gmOpponentName gm =
    -- Waiting screen
    H.div_ [HP.class_ "w-full flex flex-col items-center"]
      [ H.div_ [HP.class_ "card p-6 w-full max-w-md text-center", style_ [("margin-top", "4em")]]
          (if gmIsMatchmaking gm
           then
             let ticks = gmMatchmakingTicks gm
             in if ticks >= 6
               then
                 -- AI fallback prompt
                 [ H.h2_ [HP.class_ "text-xl font-bold mb-4"] [text "No opponents found"]
                 , H.div_ [HP.class_ "text-muted-foreground text-sm mb-6"]
                     [text "Couldn't find an opponent for you."]
                 , H.button_
                     [ HP.class_ "btn bg-green-600 hover:bg-green-700 text-white border-green-500 w-full mb-3"
                     , style_ [("touch-action", "manipulation")]
                     , SVG.onClick GAcceptAiFallback
                     ]
                     [ text "Play vs AI" ]
                 , H.button_
                     [ HP.class_ "btn btn-outline text-foreground w-full mb-3"
                     , style_ [("touch-action", "manipulation")]
                     , SVG.onClick GKeepSearching
                     ]
                     [ text "Keep Searching" ]
                 , H.button_
                     [ HP.class_ "text-sm text-muted-foreground hover:text-foreground bg-transparent border-0 cursor-pointer"
                     , style_ [("touch-action", "manipulation")]
                     , SVG.onClick GCancelMatchmaking
                     ]
                     [ text "Cancel" ]
                 ]
               else if gmInterestShown gm
               then
                 -- Someone is viewing the game details
                 [ H.h2_ [HP.class_ "text-xl font-bold mb-4"] [text "Someone is interested!"]
                 , H.div_ [HP.class_ "flex justify-center mb-4"]
                     [ H.div_
                         [ HP.class_ "animate-spin rounded-full border-4 border-muted border-t-green-500"
                         , style_ [("width", "3rem"), ("height", "3rem")]
                         ] []
                     ]
                 , H.div_ [HP.class_ "text-muted-foreground text-sm mb-4"]
                     [text "Waiting for them to accept..."]
                 , H.button_
                     [ HP.class_ "btn btn-outline text-foreground mb-4"
                     , style_ [("touch-action", "manipulation")]
                     , SVG.onClick GCancelMatchmaking
                     ]
                     [ text "Cancel" ]
                 ]
               else
                 -- Matchmaking waiting UI
                 [ H.h2_ [HP.class_ "text-xl font-bold mb-4"] [text "Waiting for opponent..."]
                 , H.div_ [HP.class_ "flex justify-center mb-4"]
                     [ H.div_
                         [ HP.class_ "animate-spin rounded-full border-4 border-muted border-t-green-500"
                         , style_ [("width", "3rem"), ("height", "3rem")]
                         ] []
                     ]
                 , H.div_ [HP.class_ "text-muted-foreground text-sm mb-4"]
                     [text "Looking for players..."]
                 , H.button_
                     [ HP.class_ "btn btn-outline text-foreground mb-4"
                     , style_ [("touch-action", "manipulation")]
                     , SVG.onClick GCancelMatchmaking
                     ]
                     [ text "Cancel" ]
                 , H.div_ [HP.class_ "border-t border-border pt-4 mt-2"] []
                 , H.div_ [HP.class_ "text-xs text-muted-foreground mb-2"]
                     [text "Or share this code to invite a friend:"]
                 ] ++ case gmInviteCode gm of
                   Just code ->
                     [ H.button_
                         [HP.class_ "btn btn-outline btn-sm text-foreground"
                         , style_ [("touch-action", "manipulation")]
                         , SVG.onClick (GCopyInviteCode code)]
                         [text "Copy Link"]
                     ]
                   Nothing -> []
           else
             -- Standard invite code waiting UI
             [ H.h2_ [HP.class_ "text-xl font-bold mb-4"] [text "Waiting for opponent..."]
             , H.div_ [HP.class_ "animate-pulse text-muted-foreground mb-4"]
                 [text "Share the invite link to start"]
             ] ++ case gmInviteCode gm of
               Just code -> [ H.div_ [HP.class_ "flex flex-col gap-4 items-center"]
                 (  [ case gmQrDataUrl gm of
                        Just qr -> H.img_ [HP.src_ qr, HP.width_ "200", HP.height_ "200", HP.class_ "rounded"]
                        Nothing -> text ""
                    , H.button_
                        [HP.class_ "btn btn-outline text-foreground"
                        , style_ [("touch-action", "manipulation")]
                        , SVG.onClick (GCopyInviteCode code)]
                        [text "Copy Link"]
                    ])
                 ]
               Nothing -> []
          )
      ]
  | otherwise =
    let zen = gmViewMode gm == ZenView
        showEval = gmGameMode gm /= MultiplayerMode
        showClocks = not zen && gmTimeControl gm /= NoTimeControl && gmGameMode gm == MultiplayerMode
        showBanner = not zen && not showClocks && gmGameMode gm == MultiplayerMode
                  && isJust (gmPlayerSide gm) && isJust (gmOpponentName gm)
                  && null (gmHistory gm)
        n = boardSize (gsBoard (gmGameState gm))
        myName = case gpGuestName props of
          Just gn -> gn
          Nothing -> maybe "You" pUsername (gpProfile props)
        atkName = fromMaybe myName (gmAttackerName gm)
        defName = fromMaybe myName (gmDefenderName gm)
        turn = turnSide (gmGameState gm)
        (leftSide, leftName, leftMs, leftActive, rightSide, rightName, rightMs, rightActive) = case gmPlayerSide gm of
          Just AttackerSide ->
            ( AttackerSide, atkName, gmAttackerTimeMs gm, turn == AttackerSide
            , DefenderSide, fromMaybe "Opponent" (gmDefenderName gm), gmDefenderTimeMs gm, turn == DefenderSide )
          Just DefenderSide ->
            ( DefenderSide, defName, gmDefenderTimeMs gm, turn == DefenderSide
            , AttackerSide, fromMaybe "Opponent" (gmAttackerName gm), gmAttackerTimeMs gm, turn == AttackerSide )
          Nothing ->
            ( AttackerSide, fromMaybe "Attacker" (gmAttackerName gm), gmAttackerTimeMs gm, turn == AttackerSide
            , DefenderSide, fromMaybe "Defender" (gmDefenderName gm), gmDefenderTimeMs gm, turn == DefenderSide )
    in H.div_ [HP.class_ "w-full flex flex-col items-center"]
      [ if zen then viewZenBackdrop else text ""
      , if showClocks then viewClocks n leftSide leftName leftMs leftActive rightSide rightName rightMs rightActive (gmTimeControl gm) (gmMoveDeadline gm)
        else if showBanner then viewOpponentBanner n (fromMaybe "Opponent" (gmOpponentName gm))
        else text ""
      , H.div_
          ([HP.id_ "board-row"
          , HP.class_ ("flex flex-row items-stretch justify-center gap-2" <> if zen then " zen" else "")]
          ++ [style_ ([("margin-top", "2em") | showClocks || showBanner]
                   ++ if zen then [("position", "relative"), ("z-index", "51")] else [])])
          [ if showEval && not zen then viewEvalBar (gmEvalScore gm) else text ""
          , viewBoardPanel gm
          ]
      , if not zen && gmGameMode gm == MultiplayerMode && gmPlayerSide gm == Nothing
        then viewSpectatorBadge n (gmSpectatorCount gm) else text ""
      , if zen then text "" else viewStatus gm
      , if zen then text "" else viewOpponentNotice gm
      , if zen then text "" else viewMoveHistory gm
      , if zen then text ""
        else if gmGameMode gm == MultiplayerMode && isJust (gmPlayerSide gm)
        then viewMultiplayerControls gm else text ""
      , if zen then text "" else viewPostGamePanel gm
      , if zen then text "" else viewShareLink props gm
      , viewZenHint gm
      , if zen then text "" else viewChatToggle gm
      , if zen then text "" else viewChatPanel gm
      , if zen then text "" else viewVoiceButton gm
      , if zen then text "" else viewVoiceInviteBanner gm
      , if zen then text "" else viewVideoPiP gm
      ]

-- | Board panel with container
viewBoardPanel :: GameModel -> View GameModel GameAction
viewBoardPanel gm =
  let mAnim = if isJust (gmBrowseIndex gm) then Nothing else gmAnimateMove gm
      gm' = gm { gmGameState = displayedGameState gm
               , gmAnimateMove = mAnim
               }
      n = boardSize (gsBoard (gmGameState gm'))
  in viewBoardContainer (gmIsFullscreen gm) (gmViewMode gm == ZenView) n (viewSVGBoard gm')

-- | SVG board rendering
viewSVGBoard :: GameModel -> View GameModel GameAction
viewSVGBoard gm =
  let gs    = gmGameState gm
      board = gsBoard gs
      n     = boardSize board
      total = sqSize * n
      mAnim = gmAnimateMove gm
  in SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms total <> " " <> ms total)
    , HP.width_ "100%"
    , HP.class_ "block aspect-square"
    ]
    -- Every (r,c) position is rendered (empty squares get a keyed placeholder
    -- <g>) so the child list stays stable across moves.  When animating, the
    -- moved piece is rendered at its FROM index (preserving DOM element
    -- identity) with the TO transform, so the CSS transition fires reliably.
    ( svgDefs
    : [ renderSquareBg n r c | r <- [0..n-1], c <- [0..n-1] ]
    ++ renderSpecialSquares gs n
    ++ [ SVG.g_ [] (renderHighlights gm n) ]
    ++ [ SVG.g_ [] (renderValidDots gm n) ]
    ++ [ SVG.g_ []
         [ renderBoardSlot mAnim board n r c
         | r <- [0..n-1], c <- [0..n-1]
         ]
       ]
    ++ renderLastMove gs n
    ++ [ renderClickTarget gm n r c | r <- [0..n-1], c <- [0..n-1] ]
    )

-- | Render a single board slot.  Keeps the child list stable at n*n elements
-- so Miso always patches in place (same index, same key).  When animating,
-- the moved piece stays at its FROM index but gets the TO transform.
-- Only the FROM slot carries a CSS transition; all other pieces are static
-- so that appearing/disappearing pieces never trigger spurious animations.
renderBoardSlot :: Maybe MoveAction -> Board -> Int -> Int -> Int -> View GameModel GameAction
renderBoardSlot mAnim board _n r c =
  let k = "p-" <> ms r <> "-" <> ms c
  in case mAnim of
    Just (MoveAction from to)
      | Coords r c == from ->
        -- FROM slot: render the moved piece with CSS transition enabled
        -- so the transform change animates the slide to the TO position.
        let p = pieceAt board to
        in if p /= Empty
           then renderPiece k True (row to) (col to) p
           else SVG.g_ [key_ k] []
      | Coords r c == to ->
        -- TO slot: placeholder (the piece is rendered at the FROM slot).
        SVG.g_ [key_ k] []
    _ ->
      let p = pieceAt board (Coords r c)
      in if p /= Empty
         then renderPiece k False r c p
         else SVG.g_ [key_ k] []

-- | Render selected square highlight
renderHighlights :: GameModel -> Int -> [View GameModel GameAction]
renderHighlights gm _n = case gmSelected gm of
  Nothing -> []
  Just sc@(Coords r c) ->
    let hlColor = case pieceAt (gsBoard (gmGameState gm)) sc of
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
        ] ]

-- | Render valid move indicators
renderValidDots :: GameModel -> Int -> [View GameModel GameAction]
renderValidDots gm _n =
  let dotColor = case gmSelected gm of
        Nothing -> "rgba(80,200,120,0.6)"
        Just sc -> case pieceAt (gsBoard (gmGameState gm)) sc of
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
  | coord <- gmValidMoves gm ]

-- | Render click targets for each square
renderClickTarget :: GameModel -> Int -> Int -> Int -> View GameModel GameAction
renderClickTarget gm _n r c =
  let gs = gmGameState gm
      side = turnSide gs
      aiBlocked = (gmGameMode gm == AiMode && gmAiSide gm == side)
               || gmAiOpponent gm == Just side
      mpBlocked = gmGameMode gm == MultiplayerMode && gmPlayerSide gm /= Just side
      blocked = gmAiThinking gm || aiBlocked || mpBlocked || finished (gsResult gs)
      cur = if blocked then "default" else "pointer"
  in SVG.rect_
    [ SP.x_ (ms (c * sqSize))
    , SP.y_ (ms (r * sqSize))
    , HP.width_ (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_ "transparent"
    , style_ [("cursor", cur), ("touch-action", "manipulation")]
    , SVG.onClick (GCellClicked (Coords r c))
    ]

-- | Get the game state to display based on browse index
displayedGameState :: GameModel -> GameState
displayedGameState gm = case gmBrowseIndex gm of
  Nothing -> gmGameState gm
  Just i  -> let allStates = gmHistory gm ++ [gmGameState gm]
             in if i >= 0 && i < length allStates
                then allStates !! i
                else gmGameState gm

-- | Brief "vs Opponent" banner shown above the board before the first move,
-- occupying the same slot as the clocks.  Disappears once move history exists.
viewOpponentBanner :: Int -> MisoString -> View GameModel GameAction
viewOpponentBanner n oppName =
  H.div_
    [ HP.class_ "flex w-full justify-center items-center border border-border rounded text-sm text-muted-foreground"
    , style_ [ ("max-width", ms (sqSize * n) <> "px")
             , ("margin-top", "2em"), ("margin-bottom", "0")
             , ("padding", "0.5em 0.75em")
             ]
    ]
    [ text ("vs " <> oppName) ]

-- ---------------------------------------------------------------------------
-- Status & Controls
-- ---------------------------------------------------------------------------

-- | Game status display
viewStatus :: GameModel -> View GameModel GameAction
viewStatus gm =
  let gs     = gmGameState gm
      n      = boardSize (gsBoard gs)
      result = gsResult gs
      side   = turnSide gs
      caps   = gsCaptures gs
      isAi   = gmGameMode gm == AiMode
      isMp   = gmGameMode gm == MultiplayerMode
      myTurn = gmPlayerSide gm == Just side
      baseCls = "text-center my-4 font-bold card px-3 w-full flex justify-center items-center"
      (cls, msg)
        | finished result = case winner result of
            Just AttackerSide -> (baseCls <> " text-destructive", "Attackers win! " <> desc result)
            Just DefenderSide -> (baseCls, "Defenders win! " <> desc result)
            Nothing           -> (baseCls, "Draw! " <> desc result)
        | gmAiThinking gm = (baseCls <> " text-muted-foreground animate-pulse", "AI thinking...")
        | isAi && gmAiSide gm == side = (baseCls,
            (if side == AttackerSide then "Attacker's turn" else "Defender's turn") <> " (AI)")
        | isAi = (baseCls, "Your turn")
        | isMp && myTurn = (baseCls, "Your turn")
        | isMp && gmPlayerSide gm == Nothing = (baseCls,
            (if side == AttackerSide then "Attacker" else "Defender") <> "'s turn")
        | isMp = (baseCls <> " text-muted-foreground",
            maybe "Opponent" fromMisoString (gmOpponentName gm) <> "'s turn")
        | side == AttackerSide = (baseCls, "Attacker's turn")
        | otherwise            = (baseCls, "Defender's turn")
      borderColor
        | not (finished result) = "transparent"
        | otherwise = case winner result of
            Just AttackerSide -> "var(--piece-attacker)"
            Just DefenderSide -> "var(--piece-defender)"
            _                 -> "var(--muted-foreground)"
      capSuffix
        | finished result || null caps = ""
        | otherwise = let c = length caps
                      in " · Captured " <> ms (show c) <> if c == 1 then " piece" else " pieces"
      fullMsg = ms msg <> capSuffix
  in H.div_
    [ HP.class_ cls
    , style_ [ ("max-width", ms (sqSize * n) <> "px")
             , ("min-height", "3.5rem")
             , ("border", "1px solid " <> borderColor)
             , ("border-radius", "0.375rem") ]
    ]
    [ text (ms fullMsg) ]

-- | Spectator badge shown when watching a game you're not a player in
viewSpectatorBadge :: Int -> Int -> View GameModel GameAction
viewSpectatorBadge n count =
  H.div_
    [ HP.class_ "flex justify-center w-full mt-4"
    , style_ [("max-width", ms (sqSize * n) <> "px")]
    ]
    [ H.span_
        [ HP.class_ "text-xs text-muted-foreground tracking-widest uppercase" ]
        [ text ("Spectating" <> if count > 1
            then " \xb7 " <> ms (show count) <> " watching"
            else "") ]
    ]

-- | Share link section (shown after game finishes)
viewShareLink :: GameProps -> GameModel -> View GameModel GameAction
viewShareLink props gm =
  let result = gsResult (gmGameState gm)
  in if finished result
       then case (gmGameId gm, gpSession props) of
         (Just gid, Just sess)
           | amProvider (userAppMetadata (sessionUser sess)) /= "anonymous"
             || gmGameMode gm == MultiplayerMode
             -> viewShareSection gm gid
         _   -> text ""
       else text ""

viewShareSection :: GameModel -> MisoString -> View GameModel GameAction
viewShareSection gm gid =
  let url = "https://taflhouse.com/games/" <> gid
      n   = boardSize (gsBoard (gmGameState gm))
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
        , SVG.onClick GCopyGameLink
        ]
        [ text "Copy Link" ]
    ]

-- | Multiplayer controls (zen, resign, draw offer/accept/decline)
viewMultiplayerControls :: GameModel -> View GameModel GameAction
viewMultiplayerControls gm =
  let gs = gmGameState gm
      n = boardSize (gsBoard gs)
      finalResult = case gmFullHistory gm of
        Just fs -> gsResult (last fs)
        Nothing -> gsResult gs
      gameOver = finished finalResult
  in if gameOver then text ""
     else H.div_
       [ HP.class_ "flex items-center justify-center gap-2 mt-4"
       , style_ [("max-width", ms (sqSize * n) <> "px")]
       ]
       ([ ctrlBtn GToggleZenMode "Zen"
        , H.button_
            [ HP.class_ "btn btn-outline btn-sm text-foreground"
            , style_ [("touch-action", "manipulation")]
            , SVG.onClick GResign
            ]
            [ text "Resign" ]
        , if gmDrawOffered gm
            then H.div_
              [ HP.class_ "flex gap-1" ]
              [ H.button_
                  [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick GAcceptDraw
                  ]
                  [ text "Accept Draw" ]
              , H.button_
                  [ HP.class_ "btn btn-outline btn-sm text-foreground"
                  , style_ [("touch-action", "manipulation")]
                  , SVG.onClick GDeclineDraw
                  ]
                  [ text "Decline" ]
              ]
            else H.button_
              [ HP.class_ "btn btn-outline btn-sm text-foreground"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GOfferDraw
              ]
              [ text "Offer Draw" ]
        ])

-- | Opponent disconnect/reconnect notice banner.
viewOpponentNotice :: GameModel -> View GameModel GameAction
viewOpponentNotice gm = case gmOpponentNotice gm of
  Nothing -> text ""
  Just notice ->
    let n = boardSize (gsBoard (gmGameState gm))
        isDisconnect = notice == "Opponent disconnected"
        color = if isDisconnect then "var(--destructive)" else "#22c55e"
    in H.div_
      [ HP.class_ "text-center text-sm font-medium w-full"
      , style_ [ ("max-width", ms (sqSize * n) <> "px")
               , ("color", color)
               , ("padding", "0.25em 0")
               ]
      ]
      [ text notice ]

-- | Post-game panel with rematch controls.
-- Only shown for multiplayer players when the game is finished.
viewPostGamePanel :: GameModel -> View GameModel GameAction
viewPostGamePanel gm
  | gmGameMode gm /= MultiplayerMode = text ""
  | isNothing (gmPlayerSide gm) = text ""
  | not (finished (gsResult (gmGameState gm))) = text ""
  | gmRematchOffered gm =
    let n = boardSize (gsBoard (gmGameState gm))
    in H.div_
      [ HP.class_ "flex items-center justify-center gap-2 mt-4"
      , style_ [("max-width", ms (sqSize * n) <> "px")]
      ]
      [ H.button_
          [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
          , style_ [("touch-action", "manipulation")]
          , SVG.onClick GAcceptRematch
          ]
          [ text "Accept Rematch" ]
      , H.button_
          [ HP.class_ "btn btn-outline btn-sm text-foreground"
          , style_ [("touch-action", "manipulation")]
          , SVG.onClick GDeclineRematch
          ]
          [ text "Decline" ]
      ]
  | gmRematchPending gm =
    let n = boardSize (gsBoard (gmGameState gm))
    in H.div_
      [ HP.class_ "flex items-center justify-center mt-4"
      , style_ [("max-width", ms (sqSize * n) <> "px")]
      ]
      [ H.span_
          [ HP.class_ "text-sm text-muted-foreground animate-pulse" ]
          [ text "Rematch offer sent..." ]
      ]
  | gmOpponentOnline gm =
    let n = boardSize (gsBoard (gmGameState gm))
    in H.div_
      [ HP.class_ "flex items-center justify-center mt-4"
      , style_ [("max-width", ms (sqSize * n) <> "px")]
      ]
      [ H.button_
          [ HP.class_ "btn btn-outline btn-sm text-foreground"
          , style_ [("touch-action", "manipulation")]
          , SVG.onClick GRequestRematch
          ]
          [ text "Rematch" ]
      ]
  | otherwise =
    let n = boardSize (gsBoard (gmGameState gm))
    in H.div_
      [ HP.class_ "flex items-center justify-center mt-4"
      , style_ [("max-width", ms (sqSize * n) <> "px")]
      ]
      [ H.span_
          [ HP.class_ "text-sm text-muted-foreground" ]
          [ text "Opponent has left" ]
      ]

-- | Move history panel
viewMoveHistory :: GameModel -> View GameModel GameAction
viewMoveHistory gm
  | null (gmHistory gm) && isNothing (gmFullHistory gm) =
      -- In multiplayer as a player, Zen lives in the controls row
      if gmGameMode gm == MultiplayerMode && isJust (gmPlayerSide gm)
      then text ""
      else let n = boardSize (gsBoard (gmGameState gm))
           in H.div_
             [ HP.class_ "flex justify-center items-center w-full"
             , style_ [("max-width", ms (sqSize * n) <> "px"), ("margin-top", "0.5em")]
             ]
             [ ctrlBtn GToggleZenMode "Zen" ]
  | otherwise =
      let displayStates = gmHistory gm ++ [gmGameState gm]
          n = boardSize (gsBoard (gmGameState gm))
          viewIdx = case gmBrowseIndex gm of
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
                (  [ ctrlBtn GToggleZenMode "Zen"
                   | not (gmGameMode gm == MultiplayerMode && isJust (gmPlayerSide gm))
                   ]
                ++ [ ctrlBtn GUndo "Undo"
                   | gmGameMode gm /= MultiplayerMode
                     || finished (gsResult (gmGameState gm))
                   ]
                )
            ]
        , H.div_
            [ HP.class_ "flex gap-0.5 overflow-y-auto p-2 w-full rounded border border-border"
            , style_ [("max-height", "10rem"), ("flex-direction", "column-reverse")]
            ]
            [ moveBtn gm i gs n (i == viewIdx)
            | (i, gs) <- reverse (zip [0..] displayStates)
            ]
        ]

-- | Individual move button in history list
moveBtn :: GameModel -> Int -> GameState -> Int -> Bool -> View GameModel GameAction
moveBtn gm idx gs n isCurrent =
  let moveSide = opponentSide gs  -- side that made this move
      isHuman = case gsLastAction gs of
        Nothing -> False
        Just _  -> gmGameMode gm == PracticeMode || gmAiSide gm /= moveSide
      movedPiece = case gsLastAction gs of
        Nothing            -> Empty
        Just (MoveAction _ t) -> pieceAt (gsBoard gs) t
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
      (textColor, borderLeftColor) = case movedPiece of
        Attacker -> ("var(--piece-attacker)", "var(--piece-attacker)")
        King     -> ("var(--piece-king)", "var(--piece-king)")
        Defender -> ("var(--piece-defender)", "var(--piece-defender)")
        Empty    -> ("var(--foreground)", "transparent")
      activeCls = " border-l-2"
      moveStyle = [("color", textColor), ("border-left-color", borderLeftColor)]
      boldCls = if isHuman || isCurrent then " font-bold" else ""
  in H.button_
    [ HP.class_ ("text-xs font-mono text-left w-full py-1 px-2 rounded hover:bg-muted cursor-pointer bg-transparent border-0 text-foreground" <> activeCls <> boldCls)
    , style_ (("touch-action", "manipulation") : moveStyle)
    , SVG.onClick (GGotoMove idx)
    ]
    [ text label ]

-- | Control button
ctrlBtn :: GameAction -> MisoString -> View GameModel GameAction
ctrlBtn action label =
  H.button_
    [ HP.class_ "btn btn-outline btn-sm text-foreground"
    , style_ [("touch-action", "manipulation")]
    , SVG.onClick action
    ]
    [ text label ]

-- | Full-screen backdrop behind the board in zen mode.
-- Double-clicking anywhere outside the board exits zen mode.
viewZenBackdrop :: View GameModel GameAction
viewZenBackdrop =
  H.div_
    [ style_ [ ("position", "fixed"), ("inset", "0"), ("z-index", "50") ]
    , on "dblclick" emptyDecoder (\() _ -> GToggleZenMode)
    ]
    []

-- | Zen mode hint (fixed overlay at bottom of screen)
viewZenHint :: GameModel -> View GameModel GameAction
viewZenHint gm
  | gmZenHint gm =
    H.div_
      [ HP.class_ "card px-4 py-2 text-sm text-muted-foreground shadow-lg"
      , style_ [ ("position", "fixed"), ("bottom", "1.5rem"), ("left", "50%")
               , ("transform", "translateX(-50%)"), ("z-index", "9999")
               , ("pointer-events", "none")
               ]
      ]
      [ H.span_ [ HP.class_ "hidden sm:inline" ] [ text "Double-click outside board to exit zen mode" ]
      , H.span_ [ HP.class_ "sm:hidden" ] [ text "Double-tap outside board to exit zen mode" ]
      ]
  | otherwise = text ""

-- ---------------------------------------------------------------------------
-- Chat
-- ---------------------------------------------------------------------------

-- | Chat toggle button (bottom-left corner, round icon, only in multiplayer)
viewChatToggle :: GameModel -> View GameModel GameAction
viewChatToggle gm
  | gmGameMode gm /= MultiplayerMode = text ""
  | gmChatOpen gm = text ""  -- hide toggle when panel is open
  | otherwise =
    H.div_
      [ style_ [ ("position", "fixed"), ("bottom", "1rem"), ("left", "1rem")
               , ("z-index", "50")
               ]
      ]
      [ H.button_
          [ style_ [ ("width", "44px"), ("height", "44px"), ("border-radius", "9999px")
                   , ("background", "var(--card)"), ("border", "1px solid var(--border)")
                   , ("cursor", "pointer"), ("display", "inline-flex")
                   , ("align-items", "center"), ("justify-content", "center")
                   , ("color", "var(--foreground)")
                   , ("box-shadow", "0 2px 8px rgba(0,0,0,0.12)")
                   , ("touch-action", "manipulation"), ("padding", "0")
                   ]
          , SVG.onClick GToggleChat
          , HP.title_ "Chat"
          ]
          [ iconMessageCircle ]
      , if gmChatUnread gm > 0
        then H.span_
          [ HP.class_ "text-xs font-bold text-white"
          , style_ [ ("position", "absolute"), ("top", "-4px"), ("right", "-4px")
                   , ("background", "var(--destructive)")
                   , ("min-width", "1.25rem"), ("height", "1.25rem")
                   , ("display", "inline-flex"), ("align-items", "center")
                   , ("justify-content", "center")
                   , ("padding", "0 0.3rem"), ("border-radius", "9999px")
                   ]
          ]
          [ text (ms (show (gmChatUnread gm))) ]
        else text ""
      ]

-- | Chat panel (slides up from bottom when open)
viewChatPanel :: GameModel -> View GameModel GameAction
viewChatPanel gm
  | not (gmChatOpen gm) = text ""
  | gmGameMode gm /= MultiplayerMode = text ""
  | otherwise =
    H.div_
      [ HP.class_ "card"
      , style_ [ ("position", "fixed"), ("bottom", "0"), ("left", "50%")
               , ("transform", "translateX(-50%)")
               , ("width", "22rem"), ("max-width", "100vw")
               , ("z-index", "50"), ("display", "flex")
               , ("flex-direction", "column"), ("border-radius", "0.5rem 0.5rem 0 0")
               , ("box-shadow", "0 -2px 12px rgba(0,0,0,0.15)")
               ]
      ]
      [ -- Header
        H.div_
          [ HP.class_ "flex items-center justify-between px-3 py-2 border-b border-border" ]
          [ H.span_ [ HP.class_ "text-sm font-bold" ]
              [ text (case gmPlayerSide gm of
                  Just _  -> "Player Chat"
                  Nothing -> "Spectator Chat")
              ]
          , H.div_ [ HP.class_ "flex items-center gap-2" ]
              (  (if isJust (gmPlayerSide gm)
                  then [ H.button_
                           [ HP.class_ ("text-xs px-2 py-0.5 rounded border " <>
                               if gmShowSpectatorChat gm
                               then "border-border text-foreground"
                               else "border-transparent text-muted-foreground")
                           , style_ [("touch-action", "manipulation"), ("background", "transparent")]
                           , SVG.onClick GToggleSpectatorChat
                           ]
                           [ text "Spec" ]
                       ]
                  else [])
              ++ voiceHeaderButtons gm
              ++ [ H.button_
                     [ HP.class_ "text-muted-foreground hover:text-foreground"
                     , style_ [ ("background", "transparent"), ("border", "0")
                              , ("cursor", "pointer"), ("font-size", "1.2rem")
                              , ("line-height", "1"), ("padding", "0 0.25rem")
                              , ("touch-action", "manipulation")
                              ]
                     , SVG.onClick GToggleChat
                     ]
                     [ text "\xd7" ]
                 ]
              )
          ]
      , -- Messages
        H.div_
          [ HP.class_ "overflow-y-auto px-3 py-2"
          , style_ [("max-height", "12rem"), ("min-height", "4rem")]
          ]
          (if null visible
           then [ H.span_ [HP.class_ "text-xs text-muted-foreground"] [text "No messages yet"] ]
           else map viewChatMessage visible)
      , -- Input
        H.form_
          [ HP.class_ "flex gap-2 px-3 py-2 border-t border-border"
          , H.onSubmit GSendChat
          ]
          [ H.input_
              [ HP.type_ "text"
              , HP.class_ "input input-sm flex-1 bg-transparent border border-border rounded text-foreground"
              , HP.value_ (gmChatInput gm)
              , HP.placeholder_ "Type a message..."
              , H.onInput GSetChatInput
              , style_ [("font-size", "0.85rem")]
              ]
          , H.button_
              [ HP.class_ "btn btn-outline btn-sm text-foreground"
              , style_ [("touch-action", "manipulation")]
              ]
              [ text "Send" ]
          ]
      ]
    where
      visible = visibleMessages gm

-- | Filter chat messages based on viewer role and toggle state
visibleMessages :: GameModel -> [ChatMessage]
visibleMessages gm = case gmPlayerSide gm of
  Just _  -> filter (\m -> cmChannel m == "player"
                        || (gmShowSpectatorChat gm && cmChannel m == "spectator"))
                    (gmChatMessages gm)
  Nothing -> filter (\m -> cmChannel m == "spectator") (gmChatMessages gm)

-- | Render a single chat message
viewChatMessage :: ChatMessage -> View GameModel GameAction
viewChatMessage cm =
  H.div_ [ HP.class_ "text-sm mb-1" ]
    [ H.span_ [ HP.class_ "font-bold text-foreground" ] [ text (cmSender cm) ]
    , text (" " <> cmMessage cm)
    ]

-- ---------------------------------------------------------------------------
-- Video PiP / Theater
-- ---------------------------------------------------------------------------

-- | Floating picture-in-picture overlay for video chat with theater mode.
-- Container divs are always rendered (with stable IDs for JS-created video
-- elements); visibility is toggled via display:none so Miso's VDOM diffing
-- doesn't remove JS-appended children.
--
-- The child list is always the same length (2 children: video-area + controls
-- placeholder) so Miso patches in place and never destroys JS-appended
-- <video> elements.
viewVideoPiP :: GameModel -> View GameModel GameAction
viewVideoPiP gm =
  let voiceConnected = gmVoiceState gm == VoiceConnected
      hasVideo = gmCameraOn gm || gmRemoteVideoOn gm
      isConnecting = gmVoiceState gm == VoiceConnecting
                  || gmVoiceState gm == VoiceInviteSent
      -- Show PiP when: connected (with or without video) or connecting
      showPiP = voiceConnected || isConnecting
      isTheater = gmVideoViewMode gm == VideoTheater
      -- Voice-only connected: compact floating pill
      voiceOnly = voiceConnected && not hasVideo && not isTheater
      -- Connecting states: small status pill
      connectingPill = isConnecting && not voiceConnected
  in if connectingPill
     then -- Small floating pill for Calling.../Connecting... states
       H.div_
         [ HP.id_ "video-overlay"
         , HP.class_ "shadow-lg"
         , style_ ([ ("position", "fixed"), ("bottom", "1rem"), ("right", "1rem")
                   , ("z-index", "52"), ("border-radius", "9999px")
                   , ("display", "flex"), ("align-items", "center"), ("gap", "0.5rem")
                   , ("padding", "0.25rem 0.75rem 0.25rem 1rem")
                   , ("background", "var(--card)"), ("border", "1px solid var(--border)")
                   ] ++ if showPiP then [] else [("display", "none")])
         ]
         [ H.span_ [ HP.class_ "text-xs text-muted-foreground animate-pulse" ]
             [ text (if gmVoiceState gm == VoiceInviteSent then "Calling..." else "Connecting...") ]
         , pipControlButton False "rgba(239,68,68,0.9)" GVoiceEnd "Cancel" iconPhoneOff
         ]
     else if voiceOnly
     then -- Compact pill-shaped widget for voice-only calls (~160x40)
       H.div_
         [ HP.id_ "video-overlay"
         , HP.class_ "shadow-lg"
         , style_ ([ ("position", "fixed"), ("bottom", "1rem"), ("right", "1rem")
                   , ("z-index", "52"), ("border-radius", "9999px")
                   , ("display", "flex"), ("align-items", "center"), ("gap", "0.5rem")
                   , ("padding", "0.25rem 0.5rem")
                   , ("background", "var(--card)"), ("border", "1px solid var(--border)")
                   , ("cursor", "grab"), ("touch-action", "none"), ("will-change", "transform")
                   ] ++ if showPiP then [] else [("display", "none")])
         ]
         [ pipControlButton (gmVoiceMuted gm)
             (if gmVoiceMuted gm then "rgba(239,68,68,0.9)" else "rgba(34,197,94,0.9)")
             GVoiceToggleMute "Toggle mute"
             (if gmVoiceMuted gm then iconMicOff else iconMic)
         , pipControlButton False "rgba(59,130,246,0.9)" GVideoToggleCamera "Toggle camera" iconCameraIcon
         , pipControlButton False "rgba(239,68,68,0.9)" GVoiceEnd "End call" iconPhoneOff
         ]
     else -- Video mode: full PiP / theater
       let containerStyles = if isTheater
             then [ ("position", "fixed"), ("inset", "0")
                  , ("z-index", "52"), ("overflow", "hidden")
                  , ("background", "#000"), ("display", "flex")
                  , ("flex-direction", "column")
                  ] ++ if showPiP then [] else [("display", "none")]
             else [ ("position", "fixed"), ("bottom", "3.5rem"), ("right", "1rem")
                  , ("z-index", "52"), ("width", "192px"), ("height", "144px")
                  , ("border-radius", "0.5rem"), ("overflow", "hidden")
                  , ("cursor", "grab"), ("touch-action", "none"), ("will-change", "transform")
                  ] ++ if showPiP then [] else [("display", "none")]
           videoAreaStyles = if isTheater
             then [ ("width", "100%"), ("flex", "1"), ("position", "relative")
                  , ("background", "#000")
                  ]
             else [ ("width", "100%"), ("height", "100%"), ("position", "relative")
                  , ("background", "var(--card)")
                  ]
           localPreviewStyles = if isTheater
             then [ ("position", "absolute"), ("bottom", "0.75rem"), ("right", "0.75rem")
                  , ("width", "128px"), ("height", "96px")
                  , ("border-radius", "0.375rem"), ("overflow", "hidden")
                  , ("border", "2px solid rgba(255,255,255,0.3)")
                  ]
             else [ ("position", "absolute"), ("bottom", "0.25rem"), ("right", "0.25rem")
                  , ("width", "64px"), ("height", "48px")
                  , ("border-radius", "0.25rem"), ("overflow", "hidden")
                  , ("border", "1px solid rgba(255,255,255,0.3)")
                  ]
       in H.div_
         [ HP.id_ "video-overlay"
         , style_ containerStyles
         , HP.class_ "shadow-lg"
         ]
         [ -- Video area (always 3 children: remote, local, expand/placeholder)
           H.div_
             [ style_ videoAreaStyles ]
             [ H.div_
                 [ HP.id_ "remote-video-pip"
                 , style_ [ ("width", "100%"), ("height", "100%")
                          , ("border-radius", "inherit")
                          ]
                 ] []
             , H.div_
                 [ HP.id_ "local-video-preview"
                 , style_ localPreviewStyles
                 ] []
             , if isTheater
               then text ""
               else -- PiP mode: overlay control bar at bottom + expand button at top
                 H.div_ []
                   [ H.button_
                       [ HP.class_ "text-white hover:text-white"
                       , style_ [ ("position", "absolute"), ("top", "0.25rem"), ("right", "0.25rem")
                                , ("background", "rgba(0,0,0,0.5)"), ("border", "0")
                                , ("border-radius", "0.25rem"), ("cursor", "pointer")
                                , ("padding", "0.15rem 0.3rem"), ("line-height", "1")
                                , ("touch-action", "manipulation")
                                ]
                       , SVG.onClick (GVideoSetViewMode VideoTheater)
                       , HP.title_ "Theater mode"
                       ]
                       [ iconMaximize ]
                   , -- Semi-transparent control bar at bottom of PiP
                     H.div_
                       [ style_ [ ("position", "absolute"), ("bottom", "0"), ("left", "0")
                                , ("right", "0"), ("display", "flex"), ("justify-content", "center")
                                , ("gap", "0.375rem"), ("padding", "0.25rem")
                                , ("background", "rgba(0,0,0,0.5)")
                                ]
                       ]
                       [ pipControlButton (gmVoiceMuted gm)
                           (if gmVoiceMuted gm then "rgba(239,68,68,0.9)" else "rgba(34,197,94,0.9)")
                           GVoiceToggleMute "Toggle mute"
                           (if gmVoiceMuted gm then iconMicOff else iconMic)
                       , pipControlButton False
                           (if gmCameraOn gm then "rgba(59,130,246,0.9)" else "rgba(100,116,139,0.7)")
                           GVideoToggleCamera "Toggle camera"
                           (if gmCameraOn gm then iconCameraIcon else iconCameraOff)
                       , pipControlButton False "rgba(239,68,68,0.9)" GVoiceEnd "End call" iconPhoneOff
                       ]
                   ]
             ]
         , -- Theater controls bar or placeholder
           if isTheater
           then H.div_
             [ HP.class_ "flex items-center px-3 py-2"
             , style_ [("background", "rgba(0,0,0,0.7)")]
             ]
             [ -- Left: PiP button
               H.button_
                 [ HP.class_ "text-xs px-2 py-0.5 rounded border border-border text-foreground"
                 , style_ [("touch-action", "manipulation"), ("background", "transparent")]
                 , SVG.onClick (GVideoSetViewMode VideoPiP)
                 ]
                 [ iconMinimize, text " PiP" ]
             , -- Center spacer + controls
               H.div_
                 [ HP.class_ "flex items-center gap-2 mx-auto" ]
                 [ H.button_
                     [ HP.class_ ("text-xs px-2 py-0.5 rounded border " <>
                         if gmVoiceMuted gm
                         then "bg-red-600 text-white border-red-500"
                         else "bg-green-600 text-white border-green-500")
                     , style_ [("touch-action", "manipulation")]
                     , SVG.onClick GVoiceToggleMute
                     ]
                     [ text (if gmVoiceMuted gm then "Unmute" else "Mute") ]
                 , H.button_
                     [ HP.class_ ("text-xs px-2 py-0.5 rounded border " <>
                         if gmCameraOn gm
                         then "bg-blue-600 text-white border-blue-500"
                         else "border-border text-foreground")
                     , style_ [("touch-action", "manipulation"), ("background", if gmCameraOn gm then "" else "transparent")]
                     , SVG.onClick GVideoToggleCamera
                     ]
                     [ text (if gmCameraOn gm then "Cam Off" else "Cam") ]
                 , H.button_
                     [ HP.class_ "text-xs px-2 py-0.5 rounded border border-border text-foreground"
                     , style_ [("touch-action", "manipulation"), ("background", "transparent")]
                     , SVG.onClick GVoiceEnd
                     ]
                     [ text "End" ]
                 ]
             , -- Right spacer (matches PiP button width for centering)
               H.div_ [ style_ [("width", "3.5rem")] ] []
             ]
           else text ""
         ]

-- | Circular icon button used inside the PiP overlay.
pipControlButton :: Bool -> MisoString -> GameAction -> MisoString -> View GameModel GameAction -> View GameModel GameAction
pipControlButton _active bg action titleText icon =
  H.button_
    [ style_ [ ("width", "28px"), ("height", "28px"), ("border-radius", "9999px")
             , ("border", "0"), ("background", bg), ("color", "#fff")
             , ("display", "inline-flex"), ("align-items", "center"), ("justify-content", "center")
             , ("cursor", "pointer"), ("padding", "0"), ("touch-action", "manipulation")
             ]
    , SVG.onClick action
    , HP.title_ titleText
    ]
    [ icon ]

-- | Maximize icon (Lucide Maximize2)
iconMaximize :: View GameModel GameAction
iconMaximize =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "14"
    , HP.height_ "14"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.polyline_ [ SP.points_ "15 3 21 3 21 9" ]
    , SVG.polyline_ [ SP.points_ "9 21 3 21 3 15" ]
    , SVG.line_ [ SP.x1_ "21", SP.y1_ "3", SP.x2_ "14", SP.y2_ "10" ]
    , SVG.line_ [ SP.x1_ "3", SP.y1_ "21", SP.x2_ "10", SP.y2_ "14" ]
    ]

-- | Minimize icon (Lucide Minimize2)
iconMinimize :: View GameModel GameAction
iconMinimize =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "14"
    , HP.height_ "14"
    , SP.fill_ "none"
    , SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    ]
    [ SVG.polyline_ [ SP.points_ "4 14 10 14 10 20" ]
    , SVG.polyline_ [ SP.points_ "20 10 14 10 14 4" ]
    , SVG.line_ [ SP.x1_ "14", SP.y1_ "10", SP.x2_ "21", SP.y2_ "3" ]
    , SVG.line_ [ SP.x1_ "3", SP.y1_ "21", SP.x2_ "10", SP.y2_ "14" ]
    ]

-- ---------------------------------------------------------------------------
-- Voice chat
-- ---------------------------------------------------------------------------

-- | Voice controls rendered inline in the chat panel header.
-- Only shown for players (not spectators).
voiceHeaderButtons :: GameModel -> [View GameModel GameAction]
voiceHeaderButtons gm
  | isNothing (gmPlayerSide gm) = []
  | otherwise = case gmVoiceState gm of
      VoiceIdle ->
        [ H.button_
            [ HP.class_ "text-muted-foreground hover:text-foreground"
            , style_ [ ("background", "transparent"), ("border", "0")
                     , ("cursor", "pointer"), ("padding", "0 0.25rem")
                     , ("touch-action", "manipulation"), ("display", "inline-flex")
                     , ("align-items", "center")
                     ]
            , SVG.onClick GVoiceInvite
            , HP.title_ "Voice chat"
            ]
            [ iconHeadset ]
        ]
      VoiceInviteSent ->
        [ H.span_
            [ HP.class_ "text-xs text-muted-foreground animate-pulse" ]
            [ text "Calling..." ]
        ]
      VoiceInviteReceived -> []  -- banner handles accept/decline
      VoiceConnecting ->
        [ H.span_
            [ HP.class_ "text-xs text-muted-foreground animate-pulse" ]
            [ text "Connecting..." ]
        ]
      VoiceConnected ->
        [ H.span_
            [ HP.class_ "text-xs text-green-500 flex items-center gap-1" ]
            [ H.span_
                [ style_ [ ("width", "6px"), ("height", "6px"), ("border-radius", "9999px")
                         , ("background", "#22c55e"), ("display", "inline-block")
                         ]
                ] []
            , text "Connected"
            ]
        ]

-- | Standalone voice button — replaced by PiP-based controls.
viewVoiceButton :: GameModel -> View GameModel GameAction
viewVoiceButton _ = text ""

-- | Headset icon (over-ear headphones with mic boom)
iconHeadset :: View GameModel GameAction
iconHeadset =
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
    [ SVG.path_ [ SP.d_ "M3 18v-6a9 9 0 0 1 18 0v6" ]
    , SVG.path_ [ SP.d_ "M21 19a2 2 0 0 1-2 2h-1a2 2 0 0 1-2-2v-3a2 2 0 0 1 2-2h3z" ]
    , SVG.path_ [ SP.d_ "M3 19a2 2 0 0 0 2 2h1a2 2 0 0 0 2-2v-3a2 2 0 0 0-2-2H3z" ]
    ]

-- | Mic icon (Lucide Mic)
iconMic :: View GameModel GameAction
iconMic =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "16", HP.height_ "16"
    , SP.fill_ "none", SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2", SP.strokeLinecap_ "round", SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z" ]
    , SVG.path_ [ SP.d_ "M19 10v2a7 7 0 0 1-14 0v-2" ]
    , SVG.line_ [ SP.x1_ "12", SP.y1_ "19", SP.x2_ "12", SP.y2_ "22" ]
    ]

-- | Mic-off icon (Lucide MicOff)
iconMicOff :: View GameModel GameAction
iconMicOff =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "16", HP.height_ "16"
    , SP.fill_ "none", SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2", SP.strokeLinecap_ "round", SP.strokeLinejoin_ "round"
    ]
    [ SVG.line_ [ SP.x1_ "2", SP.y1_ "2", SP.x2_ "22", SP.y2_ "22" ]
    , SVG.path_ [ SP.d_ "M18.89 13.23A7.12 7.12 0 0 0 19 12v-2" ]
    , SVG.path_ [ SP.d_ "M5 10v2a7 7 0 0 0 12 5" ]
    , SVG.path_ [ SP.d_ "M15 9.34V5a3 3 0 0 0-5.68-1.33" ]
    , SVG.path_ [ SP.d_ "M9 9v3a3 3 0 0 0 5.12 2.12" ]
    , SVG.line_ [ SP.x1_ "12", SP.y1_ "19", SP.x2_ "12", SP.y2_ "22" ]
    ]

-- | Camera icon (Lucide Video)
iconCameraIcon :: View GameModel GameAction
iconCameraIcon =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "16", HP.height_ "16"
    , SP.fill_ "none", SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2", SP.strokeLinecap_ "round", SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "m16 13 5.223 3.482a.5.5 0 0 0 .777-.416V7.934a.5.5 0 0 0-.777-.416L16 11" ]
    , SVG.rect_ [ SP.x_ "2", SP.y_ "7", HP.width_ "14", HP.height_ "10", SP.rx_ "2" ]
    ]

-- | Camera-off icon (Lucide VideoOff)
iconCameraOff :: View GameModel GameAction
iconCameraOff =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "16", HP.height_ "16"
    , SP.fill_ "none", SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2", SP.strokeLinecap_ "round", SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M10.66 5H14a2 2 0 0 1 2 2v2.34" ]
    , SVG.path_ [ SP.d_ "M16 11v1a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V7a2 2 0 0 1 2-2h2" ]
    , SVG.path_ [ SP.d_ "m16 13 5.223 3.482a.5.5 0 0 0 .777-.416V7.934a.5.5 0 0 0-.777-.416L16 11" ]
    , SVG.line_ [ SP.x1_ "2", SP.y1_ "2", SP.x2_ "22", SP.y2_ "22" ]
    ]

-- | Phone-off icon (Lucide PhoneOff)
iconPhoneOff :: View GameModel GameAction
iconPhoneOff =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "16", HP.height_ "16"
    , SP.fill_ "none", SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2", SP.strokeLinecap_ "round", SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M10.68 13.31a16 16 0 0 0 3.41 2.6l1.27-1.27a2 2 0 0 1 2.11-.45 12.84 12.84 0 0 0 2.81.7 2 2 0 0 1 1.72 2v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.42 19.42 0 0 1-3.33-2.67m-2.67-3.34a19.79 19.79 0 0 1-3.07-8.63A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72 12.84 12.84 0 0 0 .7 2.81 2 2 0 0 1-.45 2.11L8.09 9.91" ]
    , SVG.line_ [ SP.x1_ "22", SP.y1_ "2", SP.x2_ "2", SP.y2_ "22" ]
    ]

-- | MessageCircle icon (Lucide MessageCircle)
iconMessageCircle :: View GameModel GameAction
iconMessageCircle =
  SVG.svg_
    [ SP.viewBox_ "0 0 24 24"
    , HP.width_ "20", HP.height_ "20"
    , SP.fill_ "none", SP.stroke_ "currentcolor"
    , SP.strokeWidth_ "2", SP.strokeLinecap_ "round", SP.strokeLinejoin_ "round"
    ]
    [ SVG.path_ [ SP.d_ "M7.9 20A9 9 0 1 0 4 16.1L2 22Z" ] ]

-- | Voice invite banner (shown when receiving an invite)
viewVoiceInviteBanner :: GameModel -> View GameModel GameAction
viewVoiceInviteBanner gm
  | gmVoiceState gm /= VoiceInviteReceived = text ""
  | otherwise =
    let oppName = fromMaybe "Opponent" (gmOpponentName gm)
    in H.div_
      [ HP.class_ "card px-4 py-3 shadow-lg"
      , style_ [ ("position", "fixed"), ("bottom", "4rem"), ("left", "1rem")
               , ("z-index", "51"), ("min-width", "14rem")
               ]
      ]
      [ H.div_ [ HP.class_ "text-sm font-bold mb-2" ]
          [ text (oppName <> " wants to voice chat") ]
      , H.div_ [ HP.class_ "flex gap-2" ]
          [ H.button_
              [ HP.class_ "btn btn-sm bg-green-600 hover:bg-green-700 text-white border-green-500"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GVoiceAccept
              ]
              [ text "Accept" ]
          , H.button_
              [ HP.class_ "btn btn-outline btn-sm text-foreground"
              , style_ [("touch-action", "manipulation")]
              , SVG.onClick GVoiceDecline
              ]
              [ text "Decline" ]
          ]
      ]
