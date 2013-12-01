
module Model.Types (
    User(..)
  , ConfigVal(..)
  ) where

import qualified Data.Text as T

data User = User Int T.Text

data ConfigVal = CVString T.Text | CVDouble Double
