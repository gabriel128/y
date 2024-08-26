module Types.Defs where

data NativeType = I64 | TyBool
  deriving (Eq, Show)

data Type = Native NativeType | TyToInfer | Custom | Void
  deriving (Eq, Show)
