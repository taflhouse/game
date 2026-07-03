-- Add interest_status column to games table for matchmaking interest flow.
-- Values: NULL (no interest), 'viewing' (someone opened details), 'declined'.
ALTER TABLE games ADD COLUMN IF NOT EXISTS interest_status TEXT;
