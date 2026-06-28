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
  const audio = new Audio('/chess_move_on_alabaster.wav');
  audio.play().catch(() => {});
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

// -- Custom session restoration --

globalThis.getSupabaseSession = function(successCb, errorCb) {
  console.log("[getSupabaseSession] fetching...");
  globalThis.supabase.auth.getSession().then(({ data, error }) => {
    console.log("[getSupabaseSession] result", { data, error });
    if (error) { errorCb(error.message || 'Session error'); return; }
    if (!data || !data.session) { successCb(null); return; }
    // Validate session server-side; if the user was deleted (e.g. db reset),
    // clear the stale JWT so the app can re-authenticate.
    globalThis.supabase.auth.getUser().then(({ data: userData, error: userError }) => {
      if (userError || !userData || !userData.user) {
        console.warn("[getSupabaseSession] stale session detected, signing out");
        globalThis.supabase.auth.signOut().then(() => successCb(null)).catch(() => successCb(null));
      } else {
        successCb(data.session);
      }
    }).catch(() => {
      console.warn("[getSupabaseSession] getUser failed, clearing session");
      globalThis.supabase.auth.signOut().then(() => successCb(null)).catch(() => successCb(null));
    });
  }).catch(err => {
    console.error("[getSupabaseSession] catch", err);
    errorCb(String(err));
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

// -- Game clock / timer utilities --

globalThis.nowISO = function() { return new Date().toISOString(); };

globalThis.elapsedMs = function(isoString) {
  if (!isoString) return 0;
  return Math.max(0, Date.now() - new Date(isoString).getTime());
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

globalThis.stopGameClock = function(intervalId) {
  // Stop by specific ID if provided, otherwise stop the active clock
  if (intervalId != null) clearInterval(intervalId);
  if (globalThis._gameClockId != null) {
    clearInterval(globalThis._gameClockId);
    globalThis._gameClockId = null;
  }
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
