{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Types.Parsing where

import qualified Data.Text as T
import Types.Defs (NativeType (..), Type (..), TypeMeta)

fromTypeId :: TypeMeta -> T.Text -> Either T.Text Type
fromTypeId typeInfo tid =
  case T.toLower tid of
    "i64" -> Right $ TyNative typeInfo I64
    "u64" -> Right $ TyNative typeInfo U64
    "bool" -> Right $ TyNative typeInfo TyBool
    "()" -> Right $ TyNative typeInfo Unit
    someId -> Left $ T.pack "Not valid type " <> someId
