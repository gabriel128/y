module Utils where

import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Text.StringRandom

randVarName :: T.Text
{-# NOINLINE randVarName #-}
randVarName = unsafePerformIO $ stringRandomIO "tmp_\\d{15}"
