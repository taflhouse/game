{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef (newIORef)
import Miso (startApp, defaultEvents, getURI, uriSub, Component(..), LogLevel(..), component)
import Miso.DSL (asyncCallback, Function(..))
import Supabase.Miso.Core (successCallback, errorCallback)
import Supabase.Miso.Realtime (Channel)

import App.Model (Model(..), Screen(..), initModel)
import App.Action (Action(..))
import App.Route (parseRoute, Route(..))
import App.Update (updateModel)
import App.View (viewModel)
import App.FFI (js_getSupabaseSession, js_onDocumentDblClick, js_onKeyboardShortcut)
import App.Game.Model (GameModel, GameProps, initialGameModel)
import App.Game.Action (GameAction(..))
import App.Game.Update (updateGame)
import App.Game.View (viewGame)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = do
  channelRef <- newIORef (Nothing :: Maybe Channel)
  clockRef   <- newIORef (Nothing :: Maybe Int)
  uri <- getURI
  let screen0 = case parseRoute uri of
        PlayRoute _ -> LoadingScreen
        GameRoute _ -> LoadingScreen
        _           -> HomeScreen
      gameComp = (component initialGameModel (updateGame channelRef clockRef) viewGame)
        { mount   = Just GameMount
        , unmount = Just GameUnmount
        }
  startApp defaultEvents (app screen0 gameComp)
  where
    app s gc = Component
      { model            = initModel { mScreen = s }
      , hydrateModel     = Nothing
      , update           = updateModel
      , view             = viewModel gc
      , subs             = [ uriSub HandleURI
                           , \sink -> getURI >>= sink . HandleURI
                           , \sink -> do
                               okCb <- successCallback sink
                                 (\_ -> SessionRestored Nothing)
                                 (SessionRestored . Just)
                               errCb <- errorCallback sink
                                 (\_ -> SessionRestored Nothing)
                               js_getSupabaseSession okCb errCb
                           , \sink -> do
                               cb <- Function <$> asyncCallback (sink DocumentDblClick)
                               js_onDocumentDblClick cb
                           , \sink -> do
                               undoCb <- Function <$> asyncCallback (sink Undo)
                               js_onKeyboardShortcut undoCb
                           ]
      , styles           = []
      , scripts          = []
      , mountPoint       = Nothing
      , logLevel         = Off
      , mailbox          = \val -> Just (GameMailbox val)
      , bindings         = []
      , eventPropagation = False
      , mount            = Nothing
      , unmount          = Nothing
      }
