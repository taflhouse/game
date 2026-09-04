import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm';
import qrcode from 'https://cdn.jsdelivr.net/npm/qrcode-generator@1.4.4/+esm';

const SUPABASE_KEY = '__SUPABASE_KEY__';
const VAPID_PUBLIC_KEY = '__VAPID_PUBLIC_KEY__';

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

function applyTheme() {
  const stored = localStorage.getItem('taflhouse_theme');
  const dark = stored ? stored === 'dark' : matchMedia('(prefers-color-scheme: dark)').matches;
  document.documentElement.classList.toggle('dark', dark);
}

// mode: 'light' | 'dark' | 'system'. "system" means always follow the OS,
// so it's stored as the absence of a key rather than a snapshot of it.
globalThis.setTheme = (mode) => {
  if (mode === 'system') {
    localStorage.removeItem('taflhouse_theme');
  } else {
    localStorage.setItem('taflhouse_theme', mode);
  }
  applyTheme();
};

globalThis.getThemeMode = () => localStorage.getItem('taflhouse_theme') || 'system';

// Live-follow the OS while in "system" mode, for a tab left open across a
// day/night switch.
matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => {
  if (!localStorage.getItem('taflhouse_theme')) applyTheme();
});

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

// -- WebAudio sound synthesis --
//
// One shared AudioContext: browsers cap how many a page may hold (~6), and the
// move sound fires often enough to hit that if each call made its own.
let _actx = null;
function actx() {
  if (!_actx) {
    _actx = new (globalThis.AudioContext || globalThis.webkitAudioContext)();
  }
  if (_actx.state === 'suspended') _actx.resume().catch(() => {});
  return _actx;
}

let _noise = null;
function noiseBuffer(ctx) {
  if (!_noise) {
    const n = Math.floor(ctx.sampleRate * 0.5);
    _noise = ctx.createBuffer(1, n, ctx.sampleRate);
    const d = _noise.getChannelData(0);
    for (let i = 0; i < n; i++) d[i] = Math.random() * 2 - 1;
  }
  return _noise;
}

// A stone-on-board knock: two pitched partials plus a lowpassed onset click.
// Frequencies track the source wav - bright ~440Hz attack settling onto ~265Hz.
function knock(ctx, at, gain, f1, f2, decay, bright) {
  [[f1, decay * 0.55, 0.7], [f2, decay, 0.5]].forEach(([f, d, amp]) => {
    const osc = ctx.createOscillator();
    const g = ctx.createGain();
    osc.type = 'triangle';
    osc.frequency.setValueAtTime(f * 1.06, at);
    osc.frequency.exponentialRampToValueAtTime(f, at + 0.012);
    g.gain.setValueAtTime(0.0001, at);
    g.gain.exponentialRampToValueAtTime(Math.max(0.0002, amp * gain), at + 0.004);
    g.gain.exponentialRampToValueAtTime(0.0001, at + d);
    osc.connect(g).connect(ctx.destination);
    osc.start(at);
    osc.stop(at + d + 0.01);
  });
  const src = ctx.createBufferSource();
  src.buffer = noiseBuffer(ctx);
  const lp = ctx.createBiquadFilter();
  lp.type = 'lowpass';
  lp.frequency.value = bright;
  const g = ctx.createGain();
  g.gain.setValueAtTime(0.35 * gain, at);
  g.gain.exponentialRampToValueAtTime(0.0001, at + 0.014);
  src.connect(lp).connect(g).connect(ctx.destination);
  src.start(at, Math.random() * 0.4);
  src.stop(at + 0.02);
}

// A plain sine with a soft swell - the join chime's voice, reused for the
// capture tail so all three sounds stay in one family.
function tone(ctx, at, f, level, len) {
  const osc = ctx.createOscillator();
  const g = ctx.createGain();
  osc.type = 'sine';
  osc.frequency.value = f;
  g.gain.setValueAtTime(0.0001, at);
  g.gain.exponentialRampToValueAtTime(level, at + 0.02);
  g.gain.exponentialRampToValueAtTime(0.0001, at + len);
  osc.connect(g).connect(ctx.destination);
  osc.start(at);
  osc.stop(at + len + 0.01);
}

globalThis.playMoveSound = () => {
  try {
    const ctx = actx();
    // 150ms offset syncs with the piece movement animation, as before.
    knock(ctx, ctx.currentTime + 0.15, 0.5, 438, 265, 0.05, 1800);
  } catch (e) { /* autoplay blocked or no WebAudio */ }
};

// Capture variants. Each is a set of knocks (the landing, then the captured
// piece going off the board) plus optional tones receding after them.
// Switch by changing CAPTURE_VARIANT.
//   knock: [offset, gain, pitch, lowPartial, decay, clickBrightness]
//   tone:  [offset, pitch, level, length]
const CAPTURE_VARIANTS = {
  // second knock a fifth down - the two sit closest together
  fifth: {
    knocks: [[0.000, 0.55, 440, 294, 0.06, 1800],
             [0.085, 0.42, 294, 196, 0.11, 1400]],
    tones: [],
  },
  // second knock a full octave down and a touch slower - reads as heavier
  octave: {
    knocks: [[0.000, 0.55, 440, 294, 0.06, 1800],
             [0.090, 0.44, 220, 147, 0.14, 1300]],
    tones: [],
  },
  // one knock, then E5-C5-G4 receding in the join chime's voice
  chime: {
    knocks: [[0.000, 0.50, 415, 277, 0.07, 1700]],
    tones: [[0.06, 659.25, 0.085, 0.34],
            [0.13, 523.25, 0.075, 0.34],
            [0.20, 392.00, 0.065, 0.34]],
  },
  // knock, then D5-A4 - short and unfussy, for taking a piece
  twoNote: {
    knocks: [[0.000, 0.50, 415, 277, 0.07, 1700]],
    tones: [[0.07, 587.33, 0.09, 0.32],
            [0.16, 440.00, 0.08, 0.40]],
  },
  // knock, then A4-F4-C4 - falls further and settles lower, for losing one
  lowTail: {
    knocks: [[0.000, 0.52, 415, 277, 0.07, 1700]],
    tones: [[0.06, 440.00, 0.10, 0.36],
            [0.13, 349.23, 0.09, 0.36],
            [0.20, 261.63, 0.08, 0.42]],
  },
};
// Which variant plays depends on whose piece just left the board, so the sound
// tells you what happened without looking. Losing a piece falls further and
// lands lower than taking one.
const CAPTURE_BY_YOU  = 'twoNote';   // you took one of theirs
const CAPTURE_OF_YOURS = 'lowTail';  // they took one of yours

globalThis.playCaptureSound = (byYou) => {
  try {
    const ctx = actx();
    // Same 150ms offset as the move sound: on a capture only this fires, so the
    // first knock doubles as the moving piece landing.
    const t0 = ctx.currentTime + 0.15;
    const v = CAPTURE_VARIANTS[byYou ? CAPTURE_BY_YOU : CAPTURE_OF_YOURS];
    v.knocks.forEach(([dt, gain, f1, f2, decay, bright]) =>
      knock(ctx, t0 + dt, gain, f1, f2, decay, bright));
    v.tones.forEach(([dt, f, level, len]) => tone(ctx, t0 + dt, f, level, len));
  } catch (e) { /* autoplay blocked or no WebAudio */ }
};

// Game outcome. Win variants are selectable the same way as the capture ones.
//   tone: [offset, pitch, level, length]
const WIN_VARIANTS = {
  // the join chime carried on up - D5 A5 D6, unmistakably the same voice
  rise:    [[0.00, 587.33, 0.20, 0.42], [0.12, 880.00, 0.20, 0.42],
            [0.24, 1174.66, 0.22, 0.85]],
  // full rising major arpeggio, brightest
  fanfare: [[0.00, 587.33, 0.19, 0.34], [0.10, 739.99, 0.19, 0.34],
            [0.20, 880.00, 0.19, 0.34], [0.30, 1174.66, 0.22, 0.90]],
  // same shape a fourth lower - gentler, tops out at A5
  warm:    [[0.00, 440.00, 0.20, 0.38], [0.11, 554.37, 0.20, 0.38],
            [0.22, 659.25, 0.20, 0.38], [0.33, 880.00, 0.22, 0.90]],
  // two notes, then a soft echo of the pair
  echo:    [[0.00, 587.33, 0.22, 0.40], [0.12, 880.00, 0.24, 0.55],
            [0.42, 587.33, 0.09, 0.40], [0.54, 880.00, 0.10, 0.70]],
};
const WIN_VARIANT = 'warm';

globalThis.playWinSound = () => {
  try {
    const ctx = actx();
    const t0 = ctx.currentTime + 0.08;
    WIN_VARIANTS[WIN_VARIANT].forEach(([dt, f, level, len]) =>
      tone(ctx, t0 + dt, f, level, len));
  } catch (e) { /* autoplay blocked or no WebAudio */ }
};

// The mirror of a win: the same voice falling, slower and quieter. Losing
// shouldn't be punished with a loud noise.
globalThis.playLoseSound = () => {
  try {
    const ctx = actx();
    const t0 = ctx.currentTime + 0.08;
    [[0.00, 440.00, 0.14, 0.45], [0.16, 349.23, 0.13, 0.50],
     [0.32, 293.66, 0.13, 0.95]].forEach(([dt, f, level, len]) =>
      tone(ctx, t0 + dt, f, level, len));
  } catch (e) { /* autoplay blocked or no WebAudio */ }
};

// Draws, and endings where there is no "you" - hotseat games and spectators.
// Level, going nowhere.
globalThis.playDrawSound = () => {
  try {
    const ctx = actx();
    const t0 = ctx.currentTime + 0.08;
    tone(ctx, t0, 440.00, 0.15, 0.70);
    tone(ctx, t0 + 0.005, 587.33, 0.13, 0.70);
    tone(ctx, t0 + 0.30, 440.00, 0.09, 0.80);
  } catch (e) { /* autoplay blocked or no WebAudio */ }
};

globalThis.playJoinSound = () => {
  // Synthesised two-note chime - no asset to fetch, so it can't be delayed by
  // a cold cache at the exact moment the opponent appears.
  try {
    const ctx = actx();
    const now = ctx.currentTime;
    [[587.33, 0], [880.0, 0.12]].forEach(([freq, at]) => {
      const osc = ctx.createOscillator();
      const gain = ctx.createGain();
      osc.type = 'sine';
      osc.frequency.value = freq;
      gain.gain.setValueAtTime(0.0001, now + at);
      gain.gain.exponentialRampToValueAtTime(0.25, now + at + 0.02);
      gain.gain.exponentialRampToValueAtTime(0.0001, now + at + 0.35);
      osc.connect(gain).connect(ctx.destination);
      osc.start(now + at);
      osc.stop(now + at + 0.36);
    });
  } catch (e) { /* autoplay blocked or no WebAudio */ }
};

// -- App resume (tab foregrounded / network back) --
//
// Realtime is fire-and-forget: a channel that drops while the tab is frozen
// reconnects without replaying what it missed. One listener pair dispatches to
// whichever component is currently mounted, so nothing leaks across mounts.
let appResumeCb = null;
globalThis.onAppResume = (cb) => { appResumeCb = cb; };
globalThis.clearAppResume = () => { appResumeCb = null; };

document.addEventListener('visibilitychange', () => {
  if (document.visibilityState === 'visible' && appResumeCb) appResumeCb();
});
globalThis.addEventListener('online', () => { if (appResumeCb) appResumeCb(); });

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
  globalThis["supabase"][namespace][fnName](...args).then(({ data, error }) => {
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

  // Apply filters
  filters.forEach((filter) => {
    query = query[filter.operator](filter.column, filter.value);
  });

  // Apply fetch options if provided
  if (fetchOptions.count) {
    query = query.count(fetchOptions.count);
  }
  if (fetchOptions.head) {
    query = query.head();
  }
  if (fetchOptions.order) {
    query = query.order(fetchOptions.order.column, { ascending: fetchOptions.order.ascending });
  }
  if (fetchOptions.limit) {
    query = query.limit(fetchOptions.limit);
  }

  query.then((result) => {
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
  globalThis["supabase"]
    ["from"](from)
    [fnName](...args)
    .then(({ data, error }) => {
      if (error) errorful(error.message);
      else successful(data || []);
    }).catch((err) => {
      console.error("[runSupabaseQuery] catch", from, fnName, err);
      errorful(err.message || String(err));
    });
};

// -- Supabase RPC --

globalThis.runSupabaseRpc = function(fnName, params, successCallback, errorCallback) {
  globalThis.supabase.rpc(fnName, params).then(({ data, error }) => {
    if (error) errorCallback(error.message || String(error));
    else successCallback(data);
  }).catch((err) => {
    console.error("[runSupabaseRpc] catch", fnName, err);
    errorCallback(err.message || String(err));
  });
};

// -- Supabase Edge Functions --

globalThis.invokeEdgeFunction = function(fnName, body, successCallback, errorCallback) {
  globalThis.supabase.functions.invoke(fnName, { body }).then(({ data, error }) => {
    if (error) errorCallback(error.message || String(error));
    else successCallback(data);
  }).catch((err) => {
    console.error("[invokeEdgeFunction] catch", fnName, err);
    errorCallback(err.message || String(err));
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
      else if (status === 'CLOSED' && !channel._intentionalClose) errorCb(status);
    });
};

globalThis["removeChannel"] = function(channel) {
  // Mark the close as ours so the subscribe callback below doesn't report it
  // as a dropped channel and kick off a pointless resync.
  if (channel) { channel._intentionalClose = true; }
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
      else if (status === 'CLOSED' && !channel._intentionalClose) errorCb(status);
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
  // Clamp below 2^31 - the Haskell side receives this as a 32-bit Int on
  // wasm32, and elapsed durations past ~24.8 days would otherwise overflow
  // and wrap into a small/negative number, making stale games look "recent".
  var diff = Date.now() - new Date(isoString).getTime();
  return Math.max(0, Math.min(diff, 2000000000));
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

  // clockCb sinks a GClockTick into the WASM app, which mutates the model and
  // re-renders the whole view. Calling it on every 100ms poll meant ten full
  // re-renders a second for the length of a blitz game, which backs up the
  // app's message queue and delays both local moves and incoming realtime
  // updates. Keep polling at 100ms so timeouts stay accurate, but only call
  // back when the *displayed* value changes: tenths inside the last 10s, where
  // they're actually rendered, and whole seconds above that.
  const displayKey = (ms) => (ms < 10000 ? Math.ceil(ms / 100) : Math.ceil(ms / 1000));
  let lastKey = null;

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
    const key = displayKey(atkDisplay) + ':' + displayKey(defDisplay);
    if (key !== lastKey) {
      lastKey = key;
      clockCb(atkDisplay, defDisplay);
    }
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
      else if (status === 'CLOSED' && !channel._intentionalClose) errorCb(status);
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
    var stream = e.streams[0] || new MediaStream([e.track]);
    trackCb(e.track.kind, stream);
    if (e.track.kind === 'video') {
      e.track.onended = function() { trackCb('video-ended', null); };
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

// -- PiP drag-to-reposition --

globalThis.makePipDraggable = function() {
  var el = document.getElementById('video-overlay');
  if (!el || el._draggable) return;
  el._draggable = true;
  el.style.touchAction = 'none';
  var dx = 0, dy = 0, startX = 0, startY = 0, dragging = false;

  el.addEventListener('pointerdown', function(e) {
    if (e.target.closest('button')) return;
    e.preventDefault();
    dragging = true;
    startX = e.clientX - dx;
    startY = e.clientY - dy;
    el.setPointerCapture(e.pointerId);
    el.style.cursor = 'grabbing';
  });

  el.addEventListener('pointermove', function(e) {
    if (!dragging) return;
    dx = e.clientX - startX;
    dy = e.clientY - startY;
    el.style.transform = 'translate(' + dx + 'px,' + dy + 'px)';
  });

  el.addEventListener('pointerup', function(e) {
    if (!dragging) return;
    dragging = false;
    el.style.cursor = 'grab';
  });
};

globalThis.clearPipDragTransform = function() {
  var el = document.getElementById('video-overlay');
  if (!el) return;
  el.style.transform = '';
  el._draggable = false;
  // Remove old listeners by replacing the element with a clone
  // (not needed — we guard with _draggable flag on re-attach)
};

globalThis.voiceToggleMute = function(stream) {
  if (!stream) return true;
  var track = stream.getAudioTracks()[0];
  if (!track) return true;
  track.enabled = !track.enabled;
  return !track.enabled;
};

// -- Web Push notifications --

// Register Service Worker at load time
if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/sw.js')
    .then(function() {})
    .catch(function(err) { console.error('[push] SW registration failed:', err); });
}

function urlBase64ToUint8Array(base64String) {
  var padding = '='.repeat((4 - base64String.length % 4) % 4);
  var base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');
  var rawData = atob(base64);
  var outputArray = new Uint8Array(rawData.length);
  for (var i = 0; i < rawData.length; i++) {
    outputArray[i] = rawData.charCodeAt(i);
  }
  return outputArray;
}

globalThis.subscribeToPush = function(successCb, errorCb) {
  if (!('serviceWorker' in navigator) || !('PushManager' in window)) {
    errorCb('Push not supported');
    return;
  }
  navigator.serviceWorker.ready.then(function(reg) {
    return reg.pushManager.subscribe({
      userVisibleOnly: true,
      applicationServerKey: urlBase64ToUint8Array(VAPID_PUBLIC_KEY)
    });
  }).then(function(sub) {
    successCb(JSON.stringify(sub.toJSON()));
  }).catch(async function(err) {
    console.error('[push] subscribeToPush error:', err);
    var isBrave = navigator.brave && await navigator.brave.isBrave();
    if (isBrave && /push service/i.test(err.message)) {
      errorCb('brave_push_blocked');
    } else {
      errorCb(err.message || String(err));
    }
  });
};

globalThis.savePushSubscription = function(subJson, userId, successCb, errorCb) {
  var sub = JSON.parse(subJson);
  var row = {
    user_id: userId,
    endpoint: sub.endpoint,
    p256dh: sub.keys.p256dh,
    auth: sub.keys.auth,
    subscription_json: sub
  };
  globalThis.supabase.from('push_subscriptions')
    .upsert(row, { onConflict: 'user_id,endpoint' })
    .then(function(result) {
      if (result.error) errorCb(result.error.message);
      else successCb();
    }).catch(function(err) {
      errorCb(err.message || String(err));
    });
};

globalThis.requestNotificationPermission = function(successCb, errorCb) {
  if (!('Notification' in window)) {
    errorCb('not_supported');
    return;
  }
  if (Notification.permission === 'granted') {
    successCb('granted');
    return;
  }
  if (Notification.permission === 'denied') {
    errorCb('denied');
    return;
  }
  Notification.requestPermission().then(function(result) {
    if (result === 'granted') successCb('granted');
    else errorCb(result);
  }).catch(function(err) {
    errorCb(err.message || String(err));
  });
};

globalThis.getNotificationPermissionState = function() {
  if (!('Notification' in window)) return 'unsupported';
  return Notification.permission; // "default", "granted", or "denied"
};

globalThis.isBraveBrowser = function() {
  return !!(navigator.brave && navigator.brave.isBrave);
};

globalThis.isFirefoxBrowser = function() {
  return /Firefox\//.test(navigator.userAgent);
};

globalThis.isSafariBrowser = function() {
  return /^(?!.*Chrome).*Safari\//.test(navigator.userAgent);
};

globalThis.isEdgeBrowser = function() {
  return /Edg\//.test(navigator.userAgent);
};

globalThis.isMacOS = function() {
  return /Macintosh|Mac OS X/.test(navigator.userAgent);
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
