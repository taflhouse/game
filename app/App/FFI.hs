{-# LANGUAGE CPP #-}
module App.FFI
  ( -- * Raw FFI
    js_playMoveSound
  , js_toggleDarkMode
  , js_loadLocalGames
  , js_clearLocalGames
  , js_toggleFullscreen
  , js_onDocumentDblClick
  , js_onKeyboardShortcut
  , js_startGameClock
  , js_stopGameClock
  , js_startDailyClock
    -- * Wrapped helpers
  , js_generateUUID
  , js_copyToClipboard
  , js_getOrigin
  , js_generateQRDataURL
  , js_nowISO
  , js_elapsedMs
  , js_addSecondsISO
  , js_formatDeadline
  , js_formatDate
  , generateInviteCode
  , guestNameFromId
  , saveLocalGameIO
  ) where

import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)
import Miso.String (MisoString, ms, fromMisoString)
import Miso.JSON (Value)
import Miso.DSL (JSVal, toJSVal, fromJSValUnchecked, jsg, (#), Function(..))

-- ---------------------------------------------------------------------------
-- FFI declarations
-- ---------------------------------------------------------------------------
-- NOTE: We use JSVal instead of Function in foreign imports because the
-- GHC WASM backend's C stub generator does not emit the HsFunction type
-- when the foreign import lives outside the Main module.  Haskell wrappers
-- below re-expose the original Function-based API.

#ifdef WASM
foreign import javascript unsafe "globalThis.playMoveSound()"
  js_playMoveSound :: IO ()
foreign import javascript unsafe "globalThis.toggleTheme()"
  js_toggleDarkMode :: IO ()
foreign import javascript unsafe "globalThis.loadLocalGames($1,$2)"
  js_loadLocalGames_ffi :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.clearLocalGames()"
  js_clearLocalGames :: IO ()
foreign import javascript unsafe "globalThis.generateUUID()"
  js_generateUUID_raw :: IO JSVal
foreign import javascript unsafe "globalThis.copyToClipboard($1)"
  js_copyToClipboard_raw :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.location.origin"
  js_getOrigin_raw :: IO JSVal
foreign import javascript unsafe "globalThis.generateQRDataURL($1)"
  js_generateQRDataURL_raw :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.toggleFullscreen()"
  js_toggleFullscreen :: IO ()
foreign import javascript unsafe "globalThis.onDocumentDblClick($1)"
  js_onDocumentDblClick_ffi :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.onKeyboardShortcut($1)"
  js_onKeyboardShortcut_ffi :: JSVal -> IO ()
foreign import javascript unsafe "globalThis.nowISO()"
  js_nowISO_raw :: IO JSVal
foreign import javascript unsafe "globalThis.elapsedMs($1)"
  js_elapsedMs_raw :: JSVal -> IO Int
foreign import javascript unsafe "globalThis.addSecondsISO($1,$2)"
  js_addSecondsISO_raw :: JSVal -> Int -> IO JSVal
foreign import javascript unsafe "globalThis.startGameClock($1,$2,$3,$4,$5,$6)"
  js_startGameClock_ffi :: Int -> Int -> JSVal -> JSVal -> JSVal -> JSVal -> IO Int
foreign import javascript unsafe "globalThis.stopGameClock($1)"
  js_stopGameClock :: Int -> IO ()
foreign import javascript unsafe "globalThis.formatDeadline($1)"
  js_formatDeadline_raw :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.formatDate($1)"
  js_formatDate_raw :: JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.startDailyClock($1)"
  js_startDailyClock_ffi :: JSVal -> IO Int

js_loadLocalGames :: Function -> Function -> IO ()
js_loadLocalGames (Function a) (Function b) = js_loadLocalGames_ffi a b

js_onDocumentDblClick :: Function -> IO ()
js_onDocumentDblClick (Function a) = js_onDocumentDblClick_ffi a

js_onKeyboardShortcut :: Function -> IO ()
js_onKeyboardShortcut (Function a) = js_onKeyboardShortcut_ffi a

js_startGameClock :: Int -> Int -> JSVal -> JSVal -> Function -> Function -> IO Int
js_startGameClock a b c d (Function e) (Function f) = js_startGameClock_ffi a b c d e f

js_startDailyClock :: Function -> IO Int
js_startDailyClock (Function a) = js_startDailyClock_ffi a
#else
js_playMoveSound :: IO ()
js_playMoveSound = pure ()
js_toggleDarkMode :: IO ()
js_toggleDarkMode = pure ()
js_loadLocalGames :: Function -> Function -> IO ()
js_loadLocalGames _ _ = pure ()
js_clearLocalGames :: IO ()
js_clearLocalGames = pure ()
js_generateUUID_raw :: IO JSVal
js_generateUUID_raw = toJSVal ("00000000-0000-0000-0000-000000000000" :: MisoString)
js_copyToClipboard_raw :: JSVal -> IO ()
js_copyToClipboard_raw _ = pure ()
js_getOrigin_raw :: IO JSVal
js_getOrigin_raw = toJSVal ("http://localhost:8080" :: MisoString)
js_generateQRDataURL_raw :: JSVal -> IO JSVal
js_generateQRDataURL_raw _ = toJSVal ("" :: MisoString)
js_toggleFullscreen :: IO ()
js_toggleFullscreen = pure ()
js_onDocumentDblClick :: Function -> IO ()
js_onDocumentDblClick _ = pure ()
js_onKeyboardShortcut :: Function -> IO ()
js_onKeyboardShortcut _ = pure ()
js_nowISO_raw :: IO JSVal
js_nowISO_raw = toJSVal ("" :: MisoString)
js_elapsedMs_raw :: JSVal -> IO Int
js_elapsedMs_raw _ = pure 0
js_addSecondsISO_raw :: JSVal -> Int -> IO JSVal
js_addSecondsISO_raw v _ = pure v
js_startGameClock :: Int -> Int -> JSVal -> JSVal -> Function -> Function -> IO Int
js_startGameClock _ _ _ _ _ _ = pure 0
js_stopGameClock :: Int -> IO ()
js_stopGameClock _ = pure ()
js_formatDeadline_raw :: JSVal -> IO JSVal
js_formatDeadline_raw _ = toJSVal ("" :: MisoString)
js_formatDate_raw :: JSVal -> IO JSVal
js_formatDate_raw _ = toJSVal ("" :: MisoString)
js_startDailyClock :: Function -> IO Int
js_startDailyClock _ = pure 0
#endif

-- ---------------------------------------------------------------------------
-- Wrapped helpers
-- ---------------------------------------------------------------------------

js_generateUUID :: IO MisoString
js_generateUUID = fromJSValUnchecked =<< js_generateUUID_raw

js_copyToClipboard :: MisoString -> IO ()
js_copyToClipboard s = toJSVal s >>= js_copyToClipboard_raw

js_getOrigin :: IO MisoString
js_getOrigin = fromJSValUnchecked =<< js_getOrigin_raw

js_generateQRDataURL :: MisoString -> IO MisoString
js_generateQRDataURL s = do
  v <- toJSVal s
  fromJSValUnchecked =<< js_generateQRDataURL_raw v

js_nowISO :: IO MisoString
js_nowISO = fromJSValUnchecked =<< js_nowISO_raw

js_elapsedMs :: MisoString -> IO Int
js_elapsedMs s = toJSVal s >>= js_elapsedMs_raw

js_addSecondsISO :: MisoString -> Int -> IO MisoString
js_addSecondsISO s sec = do
  sv <- toJSVal s
  fromJSValUnchecked =<< js_addSecondsISO_raw sv sec

js_formatDeadline :: MisoString -> MisoString
js_formatDeadline s = unsafePerformIO $ do
  sv <- toJSVal s
  fromJSValUnchecked =<< js_formatDeadline_raw sv

js_formatDate :: MisoString -> MisoString
js_formatDate s = unsafePerformIO $ do
  sv <- toJSVal s
  fromJSValUnchecked =<< js_formatDate_raw sv

-- | Generate a short random invite code (8 chars from UUID).
generateInviteCode :: IO MisoString
generateInviteCode = do
  uuid <- js_generateUUID
  let str = fromMisoString uuid :: String
      code = filter (/= '-') (take 8 str)
  pure (ms code)

-- | Generate a guest display name from a user ID.
guestNameFromId :: MisoString -> MisoString
guestNameFromId uid = "Guest-" <> ms (take 8 (filter (/= '-') (fromMisoString uid :: String)))

-- | Save a game record to localStorage via the Miso DSL.
saveLocalGameIO :: Value -> IO ()
saveLocalGameIO gameData = do
  val <- toJSVal gameData
  void $ jsg "globalThis" # "saveLocalGame" $ val
