
module Model.Types (
    User(..)
  ) where

import qualified Data.Text as T

data User = User Int T.Text
