{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Types.Parsing where

import qualified Data.Text as T
import Types.Defs (NativeType (..), Type (..))

fromTypeId :: T.Text -> Either T.Text Type
fromTypeId tid =
  case T.toLower tid of
    "i64" -> Right (Native I64)
    "u64" -> Right (Native U64)
    "bool" -> Right (Native TyBool)
    "()" -> Right Unit
    someId -> Left $ T.pack "Not valid type " <> someId
