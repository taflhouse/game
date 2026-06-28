{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module App.Replay.Update (updateReplay) where

import Miso hiding ((!!))
import Miso.JSON (Value, FromJSON(..), ToJSON(..), fromJSON, Result(..), object, (.=))
import Supabase.Miso.Database (selectWithFilters, FetchOptions(..), eq)

import Tafl.Game (act, initialState)
import Tafl.AI (evaluate)

import App.JSON (GameRecord(..))
import App.Model (Model)
import App.Route (lookupVariant)
import App.Replay.Model
import App.Replay.Action

updateReplay :: ReplayAction -> Effect Model ReplayProps ReplayModel ReplayAction
updateReplay = \case
  RNoOp -> pure ()

  ReplayMount -> do
    props <- getProps
    let gameId = rpGameId props
    selectWithFilters "games" "*"
      [eq "id" gameId]
      (FetchOptions Nothing Nothing)
      RReplayLoaded RReplayLoadError

  RReplayLoaded val ->
    case fromJSON val of
      Success games -> case (games :: [GameRecord]) of
        (gr:_) ->
          case (lookupVariant (grVariant gr), grMoves gr) of
            (Just variant, Just moves) -> do
              let initial = initialState variant
                  states  = scanl act initial moves
              modify $ \m -> m
                { rmReplayGame   = Just gr
                , rmReplayStates = states
                , rmReplayIndex  = 0
                , rmEvalScore    = evaluate initial
                }
            _ -> modify $ \m -> m
              { rmReplayGame   = Just gr
              , rmReplayStates = []
              , rmReplayIndex  = 0
              }
        [] -> modify $ \m -> m { rmReplayNotFound = True }
      Error _ -> modify $ \m -> m { rmReplayNotFound = True }

  RReplayLoadError _ ->
    modify $ \m -> m { rmReplayNotFound = True }

  RGotoMove i -> do
    m <- get
    let maxIdx = length (rmReplayStates m) - 1
        idx    = max 0 (min maxIdx i)
    modify $ \x -> x { rmReplayIndex = idx
                      , rmEvalScore  = evaluate (rmReplayStates m !! idx) }

  RToggleZen ->
    mailParent $ object ["type" .= ("toggle_zen" :: String)]

  RToggleFullscreen ->
    mailParent $ object ["type" .= ("toggle_fullscreen" :: String)]

  ReplayUnmount ->
    mailParent $ object ["type" .= ("replay_unmounted" :: String)]
