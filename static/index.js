import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm';
import qrcode from 'https://cdn.jsdelivr.net/npm/qrcode-generator@1.4.4/+esm';

const SUPABASE_KEY = '__SUPABASE_KEY__';

const supabase = createClient('__SUPABASE_URL__', SUPABASE_KEY, {
  global: {
    fetch: (url, options = {}) => {
      // supabase-js sends the API key in Authorization: Bearer too, but
      // the new sb_publishable_ format is not a JWT and gets rejected.
      // Strip it when it matches the raw API key; user JWTs pass through.
      if (options.headers) {
        const h = new Headers(options.headers);
        if (h.get('Authorization') === `Bearer ${SUPABASE_KEY}`) {
          h.delete('Authorization');
        }
        return fetch(url, { ...options, headers: h });
      }
      return fetch(url, options);
    }
  }
});
globalThis.supabase = supabase;

globalThis.toggleTheme = () => {
  document.documentElement.classList.toggle('dark');
  const isDark = document.documentElement.classList.contains('dark');
  localStorage.setItem('taflhouse_theme', isDark ? 'dark' : 'light');
};

globalThis.generateUUID = function() { return crypto.randomUUID(); };

globalThis.copyToClipboard = function(text) {
  navigator.clipboard.writeText(text).catch(function() {});
};

globalThis.generateQRDataURL = function(text) {
  const qr = qrcode(0, 'M');
  qr.addData(text);
  qr.make();
  return qr.createDataURL(6, 0);
};

globalThis.toggleFullscreen = () => {
  if (document.fullscreenElement) {
    document.exitFullscreen().catch(() => {});
  } else {
    document.documentElement.requestFullscreen().catch(() => {});
  }
};

globalThis.onKeyboardShortcut = (undoCb) => {
  document.addEventListener('keydown', (e) => {
    if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') return;
    // Ctrl+Z or Cmd+Z for undo
    if ((e.ctrlKey || e.metaKey) && e.key === 'z' && !e.shiftKey) {
      e.preventDefault();
      undoCb();
    }
  });
};

globalThis.onDocumentDblClick = (cb) => {
  let clickCount = 0;
  let clickTimer = null;
  document.addEventListener('click', (e) => {
    if (!e.target.closest('svg')) { clickCount = 0; return; }
    clickCount++;
    clearTimeout(clickTimer);
    if (clickCount >= 3) {
      clickCount = 0;
      window.getSelection()?.removeAllRanges();
      cb();
    } else {
      clickTimer = setTimeout(() => { clickCount = 0; }, 500);
    }
  });
};

globalThis.playMoveSound = () => {
  // Delay sound to sync with the 150ms piece movement animation
  setTimeout(() => {
    const audio = new Audio('/chess_move_on_alabaster.wav');
    audio.play().catch(() => {});
  }, 150);
};

globalThis.animatePieceMove = (fromR, fromC, toR, toC, sqSize) => {
  requestAnimationFrame(() => {
    const el = document.getElementById('piece-' + toR + '-' + toC);
    if (!el) return;
    el.animate([
      { transform: 'translate(' + (fromC * sqSize) + 'px,' + (fromR * sqSize) + 'px)' },
      { transform: 'translate(' + (toC * sqSize) + 'px,' + (toR * sqSize) + 'px)' }
    ], { duration: 150, easing: 'ease-out' });
  });
};

// -- Supabase-miso bridge functions --

// dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
globalThis["runSupabase"] = function (
  namespace,
  fnName,
  args,
  successful,
  errorful
) {
  console.log("[runSupabase]", namespace, fnName, args);
  globalThis["supabase"][namespace][fnName](...args).then(({ data, error }) => {
    console.log("[runSupabase] result", namespace, fnName, { data, error });
    if (error) errorful(error);
    else successful(data);
  }).catch((err) => {
    console.error("[runSupabase] catch", namespace, fnName, err);
    errorful(err);
  });
};
globalThis["runSupabaseFrom"] = function (
  namespace,
  fromArg,
  fnName,
  args,
  successful,
  errorful
) {
  globalThis["supabase"][namespace]
    .from(fromArg)
    [fnName](...args)
    .then(({ data, error }) => {
      if (data) successful(data);
      if (error) errorful(error);
    });
};

// Handle update queries with filters
// Called from Haskell as: runSupabaseUpdate(table, values, args, successful, errorful)
// where args = [values_, filters_, updateOptions_]
globalThis["runSupabaseUpdate"] = function (
  table,
  values,
  args,
  successful,
  errorful
) {
  const filters = args[1] || [];
  const options = args[2] || {};

  let query = globalThis["supabase"].from(table).update(values, options);

  // Apply each filter sequentially
  filters.forEach((filter) => {
    query = query[filter.operator](filter.column, filter.value);
  });

  query.then(({ data, error }) => {
    if (error) errorful(error);
    else successful(data);
  }).catch((err) => {
    errorful(err);
  });
};

// Helper function for running select queries with filters
globalThis.runSupabaseSelect = function (
  table,
  columns,
  args,
  successCallback,
  errorCallback
) {
  let query = globalThis.supabase.from(table).select(columns);

  const filters = args[0] || [];
  const fetchOptions = args[1] || {};

  console.log("[runSupabaseSelect]", table, columns, "filters:", JSON.stringify(filters), "fetchOptions:", fetchOptions);

  // Apply filters
  filters.forEach((filter) => {
    console.log("[runSupabaseSelect] applying filter:", filter.operator, filter.column, JSON.stringify(filter.value));
    query = query[filter.operator](filter.column, filter.value);
  });

  // Apply fetch options if provided
  if (fetchOptions.count) {
    query = query.count(fetchOptions.count);
  }
  if (fetchOptions.head) {
    query = query.head();
  }

  query.then((result) => {
    console.log("[runSupabaseSelect] result", table, result);
    if (result.error) {
      errorCallback(result.error.message);
    } else {
      successCallback(result.data);
    }
  }).catch((err) => {
    console.error("[runSupabaseSelect] catch", table, err);
    errorCallback(err.message || String(err));
  });
};

// Helper function for running delete queries with filters
globalThis.runSupabaseDelete = function (
  table,
  args,
  successCallback,
  errorCallback
) {
  let query = globalThis.supabase.from(table).delete();

  const filters = args[0] || [];
  const deleteOptions = args[1] || {};

  // Apply filters
  filters.forEach((filter) => {
    query = query[filter.operator](filter.column, filter.value);
  });

  // Apply delete options if provided
  if (deleteOptions.count) {
    query = query.count(deleteOptions.count);
  }

  query.then((result) => {
    if (result.error) {
      errorCallback(result.error.message);
    } else {
      successCallback(result.data);
    }
  });
};

globalThis["runSupabaseQuery"] = function (
  from,
  fnName,
  args,
  successful,
  errorful
) {
  console.log("[runSupabaseQuery]", from, fnName, JSON.stringify(args));
  globalThis["supabase"]
    ["from"](from)
    [fnName](...args)
    .then(({ data, error }) => {
      console.log("[runSupabaseQuery] result", from, fnName, "data:", data, "error:", error ? JSON.stringify(error) : null);
      if (error) errorful(error.message);
      else successful(data || []);
    }).catch((err) => {
      console.error("[runSupabaseQuery] catch", from, fnName, err);
      errorful(err.message || String(err));
    });
};

// -- Local game persistence (localStorage) --

globalThis.loadLocalGames = function(successCb, errorCb) {
  try {
    const games = JSON.parse(localStorage.getItem('taflhouse_local_games') || '[]');
    successCb(games);
  } catch (e) {
    errorCb(String(e));
  }
};

globalThis.saveLocalGame = function(gameObj) {
  const games = JSON.parse(localStorage.getItem('taflhouse_local_games') || '[]');
  gameObj.played_at = new Date().toISOString();
  games.push(gameObj);
  localStorage.setItem('taflhouse_local_games', JSON.stringify(games));
};

globalThis.clearLocalGames = function() {
  localStorage.removeItem('taflhouse_local_games');
};

// -- Supabase Realtime (Postgres Changes) --

globalThis["subscribePostgresChanges"] = function(channelName, table, filter, changeCb, subscribedCb, errorCb) {
  var opts = { event: '*', schema: 'public', table: table };
  if (filter && filter !== '') { opts.filter = filter; }
  var channel = globalThis["supabase"]
    .channel(channelName)
    .on('postgres_changes', opts, function(payload) { changeCb(payload); })
    .subscribe(function(status) {
      if (status === 'SUBSCRIBED') subscribedCb(channel);
      else if (status === 'CHANNEL_ERROR' || status === 'TIMED_OUT') errorCb(status);
    });
};

globalThis["removeChannel"] = function(channel) {
  globalThis["supabase"].removeChannel(channel);
};

globalThis["subscribePostgresChangesWithPresence"] = function(channelName, table, filter, changeCb, presenceSyncCb, subscribedCb, errorCb) {
  var opts = { event: '*', schema: 'public', table: table };
  if (filter && filter !== '') { opts.filter = filter; }
  var channel = globalThis["supabase"]
    .channel(channelName)
    .on('postgres_changes', opts, function(payload) { changeCb(payload); })
    .on('presence', { event: 'sync' }, function() {
      presenceSyncCb(channel.presenceState());
    })
    .subscribe(function(status) {
      if (status === 'SUBSCRIBED') subscribedCb(channel);
      else if (status === 'CHANNEL_ERROR' || status === 'TIMED_OUT') errorCb(status);
    });
};

globalThis["trackPresence"] = function(channel, payload) {
  channel.track(payload);
};

globalThis["untrackPresence"] = function(channel) {
  channel.untrack();
};

// -- Game clock / timer utilities --

globalThis.nowISO = function() { return new Date().toISOString(); };

globalThis.elapsedMs = function(isoString) {
  if (!isoString) return 0;
  return Math.max(0, Date.now() - new Date(isoString).getTime());
};

globalThis.formatDeadline = function(isoString) {
  if (!isoString) return '';
  const ms = new Date(isoString).getTime() - Date.now();
  if (ms <= 0) return 'expired';
  const totalSec = Math.floor(ms / 1000);
  const days = Math.floor(totalSec / 86400);
  const hours = Math.floor((totalSec % 86400) / 3600);
  const mins = Math.floor((totalSec % 3600) / 60);
  if (days > 0) return days + 'd ' + hours + 'h left';
  if (hours > 0) return hours + 'h ' + mins + 'm left';
  return mins + 'm left';
};

globalThis.formatDate = function(isoString) {
  if (!isoString) return '';
  const d = new Date(isoString);
  const months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];
  return months[d.getMonth()] + ' ' + d.getDate() + ', ' + d.getFullYear();
};

globalThis.addSecondsISO = function(isoString, seconds) {
  const d = new Date(isoString);
  d.setSeconds(d.getSeconds() + seconds);
  return d.toISOString();
};

// Active game clock interval ID (singleton — at most one clock runs at a time).
globalThis._gameClockId = null;

// Start a blitz countdown clock. Returns interval ID.
// Automatically stops any previously running clock.
// Recalculates from base values each tick to avoid drift.
globalThis.startGameClock = function(attackerMs, defenderMs, currentTurn, lastMoveAtISO, clockCb, timeoutCb) {
  // Always stop the previous clock first
  if (globalThis._gameClockId != null) {
    clearInterval(globalThis._gameClockId);
    globalThis._gameClockId = null;
  }

  const lastMoveTime = lastMoveAtISO ? new Date(lastMoveAtISO).getTime() : Date.now();

  const intervalId = setInterval(() => {
    const elapsed = Date.now() - lastMoveTime;
    let atkDisplay, defDisplay;
    if (currentTurn === 'attacker') {
      atkDisplay = Math.max(0, attackerMs - elapsed);
      defDisplay = defenderMs;
    } else {
      atkDisplay = attackerMs;
      defDisplay = Math.max(0, defenderMs - elapsed);
    }
    clockCb(atkDisplay, defDisplay);
    if (atkDisplay <= 0) { clearInterval(intervalId); globalThis._gameClockId = null; timeoutCb('attacker'); }
    else if (defDisplay <= 0) { clearInterval(intervalId); globalThis._gameClockId = null; timeoutCb('defender'); }
  }, 100);

  globalThis._gameClockId = intervalId;
  return intervalId;
};

// Start a daily countdown clock that ticks every 30s for UI refresh.
// Reuses the singleton _gameClockId so it auto-stops when a blitz clock starts.
globalThis.startDailyClock = function(tickCb) {
  if (globalThis._gameClockId != null) {
    clearInterval(globalThis._gameClockId);
    globalThis._gameClockId = null;
  }
  const intervalId = setInterval(() => { tickCb(); }, 30000);
  globalThis._gameClockId = intervalId;
  return intervalId;
};

globalThis.stopGameClock = function(intervalId) {
  // Stop by specific ID if provided, otherwise stop the active clock
  if (intervalId != null) clearInterval(intervalId);
  if (globalThis._gameClockId != null) {
    clearInterval(globalThis._gameClockId);
    globalThis._gameClockId = null;
  }
};

// -- Supabase Realtime Broadcast --

globalThis.subscribeBroadcast = function(channelName, eventName, messageCb, subscribedCb, errorCb) {
  var channel = globalThis.supabase
    .channel(channelName)
    .on('broadcast', { event: eventName }, function(payload) {
      messageCb(payload.payload);
    })
    .subscribe(function(status) {
      if (status === 'SUBSCRIBED') subscribedCb(channel);
      else if (status === 'CHANNEL_ERROR' || status === 'TIMED_OUT') errorCb(status);
    });
};

globalThis.sendBroadcast = function(channel, eventName, payload) {
  channel.send({ type: 'broadcast', event: eventName, payload: payload });
};

// -- Voice chat (WebRTC) --

globalThis.voiceGetUserMedia = function(successCb, errorCb) {
  navigator.mediaDevices.getUserMedia({ audio: true, video: false })
    .then(function(stream) { successCb(stream); })
    .catch(function(err) { errorCb(err.message || String(err)); });
};

globalThis.voiceCreatePeerConnection = function(iceCb, trackCb) {
  var pc = new RTCPeerConnection({
    iceServers: [
      { urls: 'stun:stun.l.google.com:19302' },
      { urls: 'stun:stun1.l.google.com:19302' }
    ]
  });
  pc.onicecandidate = function(e) {
    if (e.candidate) {
      iceCb(JSON.stringify(e.candidate));
    }
  };
  pc.ontrack = function(e) {
    if (e.track.kind === 'audio') {
      var audio = new Audio();
      audio.srcObject = e.streams[0] || new MediaStream([e.track]);
      audio.play().catch(function() {});
      trackCb('audio');
    } else if (e.track.kind === 'video') {
      var container = document.getElementById('remote-video-pip');
      if (container) {
        var old = document.getElementById('remote-video-element');
        if (old) old.remove();
        var video = document.createElement('video');
        video.id = 'remote-video-element';
        video.srcObject = new MediaStream([e.track]);
        video.autoplay = true;
        video.playsInline = true;
        video.muted = true;
        video.style.cssText = 'width:100%;height:100%;object-fit:cover;border-radius:inherit';
        container.appendChild(video);
      }
      trackCb('video');
      e.track.onended = function() {
        var el = document.getElementById('remote-video-element');
        if (el) el.remove();
        trackCb('video-ended');
      };
    }
  };
  return pc;
};

globalThis.voiceAddStreamToPc = function(pc, stream) {
  stream.getTracks().forEach(function(track) {
    pc.addTrack(track, stream);
  });
};

globalThis.voiceCreateOffer = function(pc, successCb, errorCb) {
  pc.createOffer()
    .then(function(offer) { return pc.setLocalDescription(offer).then(function() { return offer; }); })
    .then(function(offer) { successCb(JSON.stringify(offer)); })
    .catch(function(err) { errorCb(err.message || String(err)); });
};

globalThis.voiceCreateAnswer = function(pc, offerSdpJson, successCb, errorCb) {
  var offer = JSON.parse(offerSdpJson);
  pc.setRemoteDescription(new RTCSessionDescription(offer))
    .then(function() { return pc.createAnswer(); })
    .then(function(answer) { return pc.setLocalDescription(answer).then(function() { return answer; }); })
    .then(function(answer) { successCb(JSON.stringify(answer)); })
    .catch(function(err) { errorCb(err.message || String(err)); });
};

globalThis.voiceSetRemoteAnswer = function(pc, answerSdpJson, successCb, errorCb) {
  var answer = JSON.parse(answerSdpJson);
  pc.setRemoteDescription(new RTCSessionDescription(answer))
    .then(function() { successCb(); })
    .catch(function(err) { errorCb(err.message || String(err)); });
};

globalThis.voiceAddIceCandidate = function(pc, candidateJson, successCb, errorCb) {
  var candidate = JSON.parse(candidateJson);
  pc.addIceCandidate(new RTCIceCandidate(candidate))
    .then(function() { successCb(); })
    .catch(function(err) { errorCb(err.message || String(err)); });
};

globalThis.voiceTeardown = function(pc, stream) {
  if (pc) { try { pc.close(); } catch(e) {} }
  if (stream) { stream.getTracks().forEach(function(t) { t.stop(); }); }
  var rv = document.getElementById('remote-video-element');
  if (rv) rv.remove();
  var lv = document.getElementById('local-video-element');
  if (lv) lv.remove();
};

globalThis.voiceGetVideoMedia = function(successCb, errorCb) {
  navigator.mediaDevices.getUserMedia({ video: true, audio: false })
    .then(function(stream) { successCb(stream); })
    .catch(function(err) { errorCb(err.message || String(err)); });
};

globalThis.voiceAddVideoToPc = function(pc, stream) {
  stream.getVideoTracks().forEach(function(track) {
    pc.addTrack(track, stream);
  });
};

globalThis.voiceRemoveVideoFromPc = function(pc) {
  pc.getSenders().forEach(function(sender) {
    if (sender.track && sender.track.kind === 'video') {
      pc.removeTrack(sender);
    }
  });
};

globalThis.voiceStopVideoStream = function(stream) {
  stream.getTracks().forEach(function(t) { t.stop(); });
};

globalThis.voiceAttachLocalVideo = function(stream) {
  var container = document.getElementById('local-video-preview');
  if (!container) return;
  var old = document.getElementById('local-video-element');
  if (old) old.remove();
  var video = document.createElement('video');
  video.id = 'local-video-element';
  video.srcObject = stream;
  video.autoplay = true;
  video.playsInline = true;
  video.muted = true;
  video.style.cssText = 'width:100%;height:100%;object-fit:cover;border-radius:inherit;transform:scaleX(-1)';
  container.appendChild(video);
};

globalThis.voiceDetachLocalVideo = function() {
  var el = document.getElementById('local-video-element');
  if (el) el.remove();
};

globalThis.voiceToggleMute = function(stream) {
  if (!stream) return true;
  var track = stream.getAudioTracks()[0];
  if (!track) return true;
  track.enabled = !track.enabled;
  return !track.enabled;
};

// -- WASI / WASM loading --

import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import ghc_wasm_jsffi from "/ghc_wasm_jsffi.js";

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("/app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start(globalThis.example);
