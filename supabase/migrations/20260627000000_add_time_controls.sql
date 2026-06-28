-- Add time control columns to the games table for blitz and daily modes.
ALTER TABLE games ADD COLUMN time_control TEXT;                    -- 'blitz' | 'daily' | NULL (untimed)
ALTER TABLE games ADD COLUMN attacker_time_remaining_ms BIGINT;   -- blitz: ms remaining
ALTER TABLE games ADD COLUMN defender_time_remaining_ms BIGINT;   -- blitz: ms remaining
ALTER TABLE games ADD COLUMN last_move_at TIMESTAMPTZ;            -- when last move was made
ALTER TABLE games ADD COLUMN move_deadline TIMESTAMPTZ;           -- daily: deadline for current move
ALTER TABLE games ADD COLUMN time_per_move_seconds INTEGER;       -- daily: seconds per move (reference)
ALTER TABLE games ADD COLUMN time_per_player_ms BIGINT;           -- blitz: total time per player (reference)
