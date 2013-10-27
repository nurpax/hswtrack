
module Model.Types (
    User(..)
  ) where

import qualified Data.Text as T
import           Data.Time (UTCTime)

data User = User Int T.Text
