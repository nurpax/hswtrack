{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Model.Types (
    User(..)
  , RowId(..)
  , WeightSample(..)
  , ConfigVal(..)
  , Note(..)
  , Workout(..)
  , Exercise(..)
  , ExerciseType(..)
  , ExerciseSet(..)
  , exerciseTypeToText
  , textToExerciseType
  ) where

import           Data.Int
import qualified Data.Text as T
import           Data.Time (UTCTime)

newtype RowId = RowId { unRowId :: Int64 }
  deriving (Eq, Ord, Num, Integral, Real, Enum)

data User = User Int T.Text

data ConfigVal = CVString T.Text | CVDouble Double

data WeightSample = WeightSample {
    wsId     :: RowId
  , wsDate   :: UTCTime
  , wsWeight :: Double
  }

data Note = Note Int UTCTime T.Text

data Workout = Workout {
    workoutId        :: RowId
  , workoutUserId    :: RowId
  , workoutTimestamp :: UTCTime
  , workoutComment   :: Maybe T.Text
  , workoutPublic    :: Bool
  , workoutSets      :: [(Exercise, [ExerciseSet])]
  }

data Exercise = Exercise {
    exerciseId   :: RowId
  , exerciseName :: T.Text
  , exerciseType :: ExerciseType
  }

data ExerciseType =
    ETWeightOnly    -- Exercise is never unweighted, e.g. barbell exercises
  | ETBodyweight    -- Exercise can be bodyweight or BW + extra
  | ETTime          -- Exercise is body-weight and the metric is time in seconds (e.g. planking)

data ExerciseSet = ExerciseSet {
    exSetId        :: RowId
  , exSetTimestamp :: UTCTime
  , exSetReps      :: Int
  , exSetWeight    :: Double -- can also be duration, not just kg
  , exSetComment   :: Maybe T.Text
  }

exerciseTypeToText :: ExerciseType -> T.Text
exerciseTypeToText ETWeightOnly   = "W"
exerciseTypeToText ETBodyweight   = "BW"
exerciseTypeToText ETTime         = "T"

textToExerciseType :: T.Text -> Either String ExerciseType
textToExerciseType "W"  = Right ETWeightOnly
textToExerciseType "BW" = Right ETBodyweight
textToExerciseType "T"  = Right ETTime
textToExerciseType s    = Left ("unknown exercise type input '" ++ T.unpack s ++ "'")
