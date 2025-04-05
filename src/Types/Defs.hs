module Types.Defs where

data NativeType = I64 | U64 | TyBool
  deriving (Eq, Show)

data Type = Native NativeType | TyToInfer | Unit
  deriving (Eq, Show)
