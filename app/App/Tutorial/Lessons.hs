{-# LANGUAGE OverloadedStrings #-}
module App.Tutorial.Lessons
  ( -- * Types (re-exported from Types)
    TutorialModule(..)
  , moduleSlug
  , HighlightStyle(..)
  , HighlightSquare(..)
  , StepKind(..)
  , TutorialStep(..)
  , TutorialLesson(..)
    -- * Registry
  , allLessons
  , lookupLesson
  , moduleLessons
  ) where

import Miso.String (MisoString)

import App.Tutorial.Lessons.Types
import App.Tutorial.Lessons.Beginner (beginnerLessons)
import App.Tutorial.Lessons.Intermediate (intermediateLessons)
import App.Tutorial.Lessons.Advanced (advancedLessons)

-- ---------------------------------------------------------------------------
-- Registry
-- ---------------------------------------------------------------------------

allLessons :: [TutorialLesson]
allLessons = beginnerLessons ++ intermediateLessons ++ advancedLessons

lookupLesson :: MisoString -> Maybe TutorialLesson
lookupLesson lid = case filter (\l -> tlId l == lid) allLessons of
  (l:_) -> Just l
  []    -> Nothing

moduleLessons :: TutorialModule -> [TutorialLesson]
moduleLessons m = filter (\l -> tlModule l == m) allLessons
