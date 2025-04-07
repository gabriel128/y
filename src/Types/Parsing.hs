{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Types.Parsing where

import qualified Data.Text as T
import Types.Defs (NativeType (..), Type (..), TypeInfo)

fromTypeId :: TypeInfo -> T.Text -> Either T.Text Type
fromTypeId typeInfo tid =
  case T.toLower tid of
    "i64" -> Right (Native typeInfo I64)
    "u64" -> Right (Native typeInfo U64)
    "bool" -> Right (Native typeInfo TyBool)
    "()" -> Right Unit
    someId -> Left $ T.pack "Not valid type " <> someId
