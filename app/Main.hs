{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Miso (startApp, defaultEvents, getURI, uriSub, Component(..), LogLevel(..))
import Miso.DSL (asyncCallback, Function(..))
import Supabase.Miso.Core (successCallback, errorCallback)

import App.Model (Model(..), Screen(..), initModel)
import App.Action (Action(..))
import App.Route (parseRoute, Route(..))
import App.Update (updateModel)
import App.View (viewModel)
import App.FFI (js_getSupabaseSession, js_onDocumentDblClick, js_onKeyboardShortcut)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = do
  uri <- getURI
  let screen0 = case parseRoute uri of
        PlayRoute _ -> LoadingScreen
        GameRoute _ -> LoadingScreen
        _           -> HomeScreen
  startApp defaultEvents (app screen0)
  where
    app s = Component
      { model            = initModel { mScreen = s }
      , hydrateModel     = Nothing
      , update           = updateModel
      , view             = viewModel
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
      , mailbox          = const Nothing
      , bindings         = []
      , eventPropagation = False
      , mount            = Nothing
      , unmount          = Nothing
      }
