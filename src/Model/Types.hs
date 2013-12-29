{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Types (
    User(..)
  , RowId(..)
  , ConfigVal(..)
  , Note(..)
  , Workout(..)
  , Exercise(..)
  , ExerciseSet(..)
  ) where

import           Data.Int
import qualified Data.Text as T
import           Data.Time (UTCTime)

newtype RowId = RowId { unRowId :: Int64 }
  deriving (Eq, Ord, Num, Integral, Real, Enum)

data User = User Int T.Text

data ConfigVal = CVString T.Text | CVDouble Double

data Note = Note Int UTCTime T.Text

data Workout = Workout {
    workoutId        :: RowId
  , workoutTimestamp :: UTCTime
  , workoutComment   :: Maybe T.Text
  , workoutSets      :: [(Exercise, [ExerciseSet])]
  }

data Exercise = Exercise {
    exerciseId   :: RowId
  , exerciseName :: T.Text
  }

data ExerciseSet = ExerciseSet {
    exSetId        :: RowId
  , exSetTimestamp :: UTCTime
  , exSetReps      :: Int
  , exSetWeight    :: Double
  , exSetComment   :: Maybe T.Text
  }
