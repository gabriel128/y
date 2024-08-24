module Types.Defs where

data NativeType = TyInt | TyBool
  deriving (Eq, Show)

data Type = Native NativeType | TyToInfer | Custom | Void
  deriving (Eq, Show)
