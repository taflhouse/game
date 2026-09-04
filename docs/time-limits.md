# Time limits and how games end on the clock

## Decision

When time runs out and neither win condition has been met — the king has not
reached a corner and the attackers have not captured him — the game goes to the
**defender**, not to a draw. The siege failed.

This only matters if a *shared* game limit is ever added. With the per-player
clocks the app uses today, the question does not come up.

## What the code does today

The clock is per-player. `BlitzControl` holds milliseconds per player
(`app/App/Model.hs:58`), and `startGameClock(attackerMs, defenderMs, …)`
(`static/index.js:661`) counts down whichever side is on move, firing
`timeoutCb` with the side that hit zero.

`GClockTimeout` (`app/App/Game/Update.hs:846`) makes the side whose flag fell the
loser:

```haskell
let loserSide  = if sideStr' == "attacker" then AttackerSide else DefenderSide
    winnerSide = if sideStr' == "attacker" then DefenderSide else AttackerSide
    resultDesc = sideStr loserSide <> " lost on time"
```

So "time is up" is never ambiguous. If the attackers burn their clock without
capturing the king, the defender wins — that *is* the failed-attack outcome,
reached by the ordinary chess convention.

The engine's only draw is threefold repetition (`src/Tafl/Game.hs:82`,
`GameResult True Nothing "Draw on repetition"`). Nothing else produces
`winner = Nothing`.

## Why the defender, and not a draw

- **Tafl is asymmetric in a way chess is not.** Attackers start with roughly 2:1
  material and have exactly one win condition: capture the king. The defender's
  position *is* survival. A siege that has not broken through when the limit hits
  is a siege that failed.
- **A draw rule rewards the stronger side for stalling.** The attacker holds the
  material edge, so "shuffle safely and bank the half point" becomes a live
  strategy. The attacker is the side that should be forced to commit.
- **It matches the existing terminal rules.** `Tafl.Game` already resolves
  "this side cannot make progress" against that side — `No legal moves!`
  (`src/Tafl/Game.hs:95`) awards the win to the opponent rather than calling a
  draw.

## The caveat: defender stalling

Defender-wins on a shared limit invites the mirror abuse — the defender turtles
and runs the clock down. Two things keep that honest:

1. **Per-player clocks.** Turtling burns your own time, so it is self-defeating.
2. **The repetition rule.** `checkRepetition` (`src/Tafl/Game/Symmetry.hs:76`)
   already punishes a defender who shuffles between the same positions;
   `repetitionTurnLimit` defaults to 3 (`src/Tafl/Rules.hs:40`).

If a shared or total-game limit is ever introduced, keep the repetition check
active under it, or the defender gains a free stalling line.

## Rating consequences

`GameResult` supports a real draw (`winner :: Maybe Side`,
`src/Tafl/Game/State.hs:25`), but that should stay reserved for repetition.
Clock draws would flow into the Glicko update as half-points — a rating design
decision to make deliberately rather than inherit from a rules change.
