module Passes.Error where

import Data.Text (Text)

data PassError
  = PassError Text
  | GenericError Text
  deriving (Show)

