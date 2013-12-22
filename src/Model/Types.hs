
module Model.Types (
    User(..)
  , ConfigVal(..)
  , Note(..)
  ) where

import qualified Data.Text as T
import           Data.Time (UTCTime)

data User = User Int T.Text

data ConfigVal = CVString T.Text | CVDouble Double

data Note = Note Int UTCTime T.Text
