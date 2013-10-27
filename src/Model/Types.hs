
module Model.Types (
    User(..)
  , Comment(..)
  ) where

import qualified Data.Text as T
import           Data.Time (UTCTime)

data User = User Int T.Text

data Comment = Comment
  {
    commentId :: Int
  , commentSavedOn :: UTCTime
  , commentText :: T.Text
  } deriving (Show)

