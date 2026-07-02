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
  , js_animatePieceMove
    -- * Voice / Broadcast FFI
  , js_subscribeBroadcast
  , js_sendBroadcast
  , js_voiceGetUserMedia
  , js_voiceCreatePeerConnection
  , js_voiceAddStreamToPc
  , js_voiceCreateOffer
  , js_voiceCreateAnswer
  , js_voiceSetRemoteAnswer
  , js_voiceAddIceCandidate
  , js_voiceTeardown
  , js_voiceToggleMute
    -- * Video FFI
  , js_voiceGetVideoMedia
  , js_voiceAddVideoToPc
  , js_voiceRemoveVideoFromPc
  , js_voiceStopVideoStream
  , js_voiceAttachLocalVideo
  , js_voiceDetachLocalVideo
  , js_playAudioFromStream
  , js_createRemoteVideo
  , js_removeRemoteVideo
    -- * PiP drag
  , js_makePipDraggable
  , js_clearPipDragTransform
    -- * Supabase RPC
  , js_runSupabaseRpc
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
foreign import javascript unsafe "globalThis.animatePieceMove($1,$2,$3,$4,$5)"
  js_animatePieceMove :: Int -> Int -> Int -> Int -> Int -> IO ()

-- Voice / Broadcast
foreign import javascript unsafe "globalThis.subscribeBroadcast($1,$2,$3,$4,$5)"
  js_subscribeBroadcast_ffi :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.sendBroadcast($1,$2,$3)"
  js_sendBroadcast_ffi :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceGetUserMedia($1,$2)"
  js_voiceGetUserMedia_ffi :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceCreatePeerConnection($1,$2)"
  js_voiceCreatePeerConnection_ffi :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "globalThis.voiceAddStreamToPc($1,$2)"
  js_voiceAddStreamToPc :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceCreateOffer($1,$2,$3)"
  js_voiceCreateOffer_ffi :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceCreateAnswer($1,$2,$3,$4)"
  js_voiceCreateAnswer_ffi :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceSetRemoteAnswer($1,$2,$3,$4)"
  js_voiceSetRemoteAnswer_ffi :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceAddIceCandidate($1,$2,$3,$4)"
  js_voiceAddIceCandidate_ffi :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceTeardown($1,$2)"
  js_voiceTeardown :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "globalThis.voiceToggleMute($1)"
  js_voiceToggleMute_raw :: JSVal -> IO Bool

-- Video
foreign import javascript unsafe "globalThis.voiceGetVideoMedia($1,$2)"
  js_voiceGetVideoMedia_ffi2 :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$2.getVideoTracks().forEach(function(t){$1.addTrack(t,$2)})"
  js_voiceAddVideoToPc :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1.getSenders().forEach(function(s){if(s.track&&s.track.kind==='video')$1.removeTrack(s)})"
  js_voiceRemoveVideoFromPc :: JSVal -> IO ()
foreign import javascript unsafe "$1.getTracks().forEach(function(t){t.stop()})"
  js_voiceStopVideoStream :: JSVal -> IO ()
foreign import javascript unsafe "var c=document.getElementById('local-video-preview');if(c){var o=document.getElementById('local-video-element');if(o)o.remove();var v=document.createElement('video');v.id='local-video-element';v.srcObject=$1;v.autoplay=true;v.playsInline=true;v.muted=true;v.style.cssText='width:100%;height:100%;object-fit:cover;border-radius:inherit;transform:scaleX(-1)';c.appendChild(v)}"
  js_voiceAttachLocalVideo :: JSVal -> IO ()
foreign import javascript unsafe "var e=document.getElementById('local-video-element');if(e)e.remove()"
  js_voiceDetachLocalVideo :: IO ()
foreign import javascript unsafe "var a=new Audio();a.srcObject=$1;a.play().catch(function(){})"
  js_playAudioFromStream :: JSVal -> IO ()
foreign import javascript unsafe "var c=document.getElementById('remote-video-pip');if(c){var o=document.getElementById('remote-video-element');if(o)o.remove();var v=document.createElement('video');v.id='remote-video-element';v.srcObject=$1;v.autoplay=true;v.playsInline=true;v.muted=true;v.style.cssText='width:100%;height:100%;object-fit:cover;border-radius:inherit';c.appendChild(v)}"
  js_createRemoteVideo :: JSVal -> IO ()
foreign import javascript unsafe "var e=document.getElementById('remote-video-element');if(e)e.remove()"
  js_removeRemoteVideo :: IO ()

-- PiP drag
foreign import javascript unsafe "globalThis.makePipDraggable()"
  js_makePipDraggable :: IO ()
foreign import javascript unsafe "globalThis.clearPipDragTransform()"
  js_clearPipDragTransform :: IO ()

-- Supabase RPC
foreign import javascript unsafe "globalThis.runSupabaseRpc($1,$2,$3,$4)"
  js_runSupabaseRpc_ffi :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()

js_runSupabaseRpc :: MisoString -> Value -> Function -> Function -> IO ()
js_runSupabaseRpc fnName params (Function okCb) (Function errCb) = do
  fnJsv <- toJSVal fnName
  paramsJsv <- toJSVal params
  js_runSupabaseRpc_ffi fnJsv paramsJsv okCb errCb

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

js_subscribeBroadcast :: JSVal -> JSVal -> Function -> Function -> Function -> IO ()
js_subscribeBroadcast a b (Function c) (Function d) (Function e) = js_subscribeBroadcast_ffi a b c d e

js_sendBroadcast :: JSVal -> MisoString -> Value -> IO ()
js_sendBroadcast ch evtName payload = do
  evtJsv <- toJSVal evtName
  payloadJsv <- toJSVal payload
  js_sendBroadcast_ffi ch evtJsv payloadJsv

js_voiceGetUserMedia :: Function -> Function -> IO ()
js_voiceGetUserMedia (Function a) (Function b) = js_voiceGetUserMedia_ffi a b

js_voiceCreatePeerConnection :: Function -> Function -> IO JSVal
js_voiceCreatePeerConnection (Function a) (Function b) = js_voiceCreatePeerConnection_ffi a b

js_voiceCreateOffer :: JSVal -> Function -> Function -> IO ()
js_voiceCreateOffer pc (Function a) (Function b) = js_voiceCreateOffer_ffi pc a b

js_voiceCreateAnswer :: JSVal -> JSVal -> Function -> Function -> IO ()
js_voiceCreateAnswer pc sdp (Function a) (Function b) = js_voiceCreateAnswer_ffi pc sdp a b

js_voiceSetRemoteAnswer :: JSVal -> JSVal -> Function -> Function -> IO ()
js_voiceSetRemoteAnswer pc sdp (Function a) (Function b) = js_voiceSetRemoteAnswer_ffi pc sdp a b

js_voiceAddIceCandidate :: JSVal -> JSVal -> Function -> Function -> IO ()
js_voiceAddIceCandidate pc cand (Function a) (Function b) = js_voiceAddIceCandidate_ffi pc cand a b

js_voiceToggleMute :: JSVal -> IO Bool
js_voiceToggleMute = js_voiceToggleMute_raw

js_voiceGetVideoMedia :: Function -> Function -> IO ()
js_voiceGetVideoMedia (Function a) (Function b) = js_voiceGetVideoMedia_ffi2 a b

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
js_animatePieceMove :: Int -> Int -> Int -> Int -> Int -> IO ()
js_animatePieceMove _ _ _ _ _ = pure ()
-- Voice / Broadcast stubs
js_subscribeBroadcast :: JSVal -> JSVal -> Function -> Function -> Function -> IO ()
js_subscribeBroadcast _ _ _ _ _ = pure ()
js_sendBroadcast :: JSVal -> MisoString -> Value -> IO ()
js_sendBroadcast _ _ _ = pure ()
js_voiceGetUserMedia :: Function -> Function -> IO ()
js_voiceGetUserMedia _ _ = pure ()
js_voiceCreatePeerConnection :: Function -> Function -> IO JSVal
js_voiceCreatePeerConnection _ _ = toJSVal ("" :: MisoString)
js_voiceAddStreamToPc :: JSVal -> JSVal -> IO ()
js_voiceAddStreamToPc _ _ = pure ()
js_voiceCreateOffer :: JSVal -> Function -> Function -> IO ()
js_voiceCreateOffer _ _ _ = pure ()
js_voiceCreateAnswer :: JSVal -> JSVal -> Function -> Function -> IO ()
js_voiceCreateAnswer _ _ _ _ = pure ()
js_voiceSetRemoteAnswer :: JSVal -> JSVal -> Function -> Function -> IO ()
js_voiceSetRemoteAnswer _ _ _ _ = pure ()
js_voiceAddIceCandidate :: JSVal -> JSVal -> Function -> Function -> IO ()
js_voiceAddIceCandidate _ _ _ _ = pure ()
js_voiceTeardown :: JSVal -> JSVal -> IO ()
js_voiceTeardown _ _ = pure ()
js_voiceToggleMute :: JSVal -> IO Bool
js_voiceToggleMute _ = pure True
-- Video stubs
js_voiceGetVideoMedia :: Function -> Function -> IO ()
js_voiceGetVideoMedia _ _ = pure ()
js_voiceAddVideoToPc :: JSVal -> JSVal -> IO ()
js_voiceAddVideoToPc _ _ = pure ()
js_voiceRemoveVideoFromPc :: JSVal -> IO ()
js_voiceRemoveVideoFromPc _ = pure ()
js_voiceStopVideoStream :: JSVal -> IO ()
js_voiceStopVideoStream _ = pure ()
js_voiceAttachLocalVideo :: JSVal -> IO ()
js_voiceAttachLocalVideo _ = pure ()
js_voiceDetachLocalVideo :: IO ()
js_voiceDetachLocalVideo = pure ()
js_playAudioFromStream :: JSVal -> IO ()
js_playAudioFromStream _ = pure ()
js_createRemoteVideo :: JSVal -> IO ()
js_createRemoteVideo _ = pure ()
js_removeRemoteVideo :: IO ()
js_removeRemoteVideo = pure ()
-- PiP drag stubs
js_makePipDraggable :: IO ()
js_makePipDraggable = pure ()
js_clearPipDragTransform :: IO ()
js_clearPipDragTransform = pure ()
-- Supabase RPC stub
js_runSupabaseRpc :: MisoString -> Value -> Function -> Function -> IO ()
js_runSupabaseRpc _ _ _ _ = pure ()
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
