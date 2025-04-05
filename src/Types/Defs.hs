module Types.Defs where

data NativeType = I64 | U64 | TyBool
  deriving (Eq)

data Type = Native NativeType | TyToInfer | Unit
  deriving (Eq)

instance Show NativeType where
  show I64 = "i64"
  show U64 = "u64"
  show TyBool = "bool"

instance Show Type where
  show (Native ntype) = show ntype
  show TyToInfer = "undefined"
  show Unit = "Unit"
