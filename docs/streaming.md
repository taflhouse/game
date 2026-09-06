# Streaming: a transparent board overlay for OBS

## Decision

Taflhouse gets an **overlay route** — a chrome-less, transparent-background board
page designed to be dropped into OBS as a Browser Source. Streamers composite the
live board over their webcam without capturing a browser window, without a
plugin, and without signing in inside OBS.

The board fades; the pieces do not. That is the whole point, and it is the thing
the widely-used blend-mode trick cannot do.

## The effect people use today, and why it is a hack

Streamers currently overlay a chess board by taking a window or browser capture
of the site and setting the OBS source's **Blending Mode → Screen** (or Lighten).
No plugin involved; it ships with OBS.

Screen computes `1 − (1−a)(1−b)` per channel. Pure white stays pure white over
any background; pure black becomes fully transparent; everything between fades in
proportion to how dark it is. On a chess board that means the white pieces stay
crisp and the dark squares dissolve into the webcam feed.

It looks good in screenshots and it is the wrong tool. Transparency ends up bound
to **luminance rather than intent**: the dark pieces are ghosted whether or not
the streamer wants that, so half the position is hard to read. There is no knob
that says "fade the board, keep every piece solid," because Screen does not know
which pixels are board and which are pieces.

Tafl makes this worse than chess does. Attackers and defenders are distinguished
by colour, the board has dark centre and corner squares, and a ghosted attacker
next to a solid defender misrepresents the position. A luminance trick is not
acceptable as the recommended path for our own game.

## The mechanism we build on instead

**OBS Browser Sources composite with real per-pixel alpha.** If the page renders
with a transparent background, OBS layers it over the other sources correctly —
this is how every stream alert and chat widget works. Nothing needs installing.

That gives us per-element control: board squares at ~30% alpha, pieces at 100%
with an outline so they read against an arbitrary webcam background. Transparency
becomes a design decision instead of a side effect of colour.

## What already exists

Most of the parts are in the tree.

- **`viewBasicSVGBoard`** (`app/App/Board.hs:371`) renders a standalone board from
  a `GameState` and is already reused by both Replay and Tutorial. It is the
  overlay renderer; nothing new needs writing.
- **Zen mode proves the chrome-less path.** `app/App/View.hs:44` drops the navbar
  when `mViewMode == ZenView`, and `viewZenBackdrop`
  (`app/App/Game/View.hs:704`) is the only extra element. An overlay view is zen
  minus the backdrop and minus click-to-exit.
- **Realtime is wired.** `subscribeToTable` (`app/App/Update.hs:28`) over Postgres
  Changes already drives multiplayer, so the overlay updates live while the
  streamer plays in their own browser window.
- **Games are world-readable.** Since `20260622000000_public_game_recaps.sql` the
  policy is `SELECT USING (true)`.

That last point matters more than it sounds. **An OBS Browser Source carries no
cookies or session from the streamer's normal browser.** An overlay that needed
auth would mean signing in inside OBS's headless CEF instance — miserable to set
up and worse to debug. Ours needs no auth at all, because the data is already
public.

## The route

```
/overlay/<username>          -- that player's current game, whatever it is
/overlay/game/<uuid>         -- one fixed game
```

The username form is the one that makes this usable. The streamer pastes a URL
into OBS once during scene setup and never touches it again, across every game
they play for the rest of the stream. The uuid form exists for casting someone
else's game.

Resolution is a lookup on `profiles.username` → `auth.users.id`, then the most
recent `games` row where that id is `attacker_id` or `defender_id` and
`status = 'active'` (`20260621002000_multiplayer.sql:28`). When there is no
active game the page renders nothing at all — fully transparent, so an idle
overlay is invisible rather than a "no game" card sitting on the stream.

Appearance comes from query params so streamers tune it without a settings UI:

```
?board=30     board square opacity, percent
&pieces=100   piece opacity, percent
&size=640     board width in px
&flip=1       render from the defender's side
&coords=0     hide rank/file labels
&eval=1       show the eval bar
```

## The transparent background

The only genuinely fiddly part. `styles.css` paints an opaque
`--color-background`, and the page must be transparent from the very first frame
or OBS captures a flash of solid colour on every scene load.

The hook belongs in the inline `<head>` script in `static/index.html:23` —
alongside the existing pre-paint theme check, which is there for exactly the same
reason (avoiding a flash before the deferred module runs). Something like:

```js
if (location.pathname.startsWith('/overlay/')) {
  document.documentElement.classList.add('overlay');
}
```

...with `.overlay, .overlay body { background: transparent !important; }` in the
stylesheet. Doing it from `static/index.js` is too late: it is a `type="module"`
script and therefore deferred.

## OBS setup notes for streamers

Worth shipping as a short help page, because two defaults will bite:

- **Uncheck "Shutdown source when not visible."** `app.wasm` is ~8.5MB; with this
  on, the entire app re-downloads every time the scene is activated.
- **Uncheck "Refresh browser when scene becomes active,"** for the same reason.
- Set the Browser Source width and height to match the `size` param, or CEF
  renders at its default resolution and OBS scales it blurry.

If load time still hurts after that, the fallback is a slim overlay-only build
that skips the routing, auth, and multiplayer machinery. Reuse the existing SPA
first and split only if measurement says to — one board renderer beats two.

## What streamers can do right now

Nothing in this document has to ship for the effect to be available today: put a
browser capture of taflhouse in zen mode into OBS and set Blending Mode → Screen.
Same result the chess streamers get, same limitation on dark pieces. The overlay
route replaces it with something that actually keeps the position readable.

## Open questions

- **Move highlighting.** On stream the audience misses moves that happen while
  the camera is on the streamer's face. A brief flash on the from/to squares
  would help, and the animation machinery already exists in the game view.
- **Overlay for spectators, not just players.** `/overlay/game/<uuid>` works for
  casting a tournament game. Does that want a distinct commentary layout — both
  clocks, both names, captured pieces — rather than the player-facing one?
- **Rate limiting.** A public overlay route is an unauthenticated read that polls
  or subscribes indefinitely. Fine at current traffic; worth watching if a
  moderately large stream ever points at it.
