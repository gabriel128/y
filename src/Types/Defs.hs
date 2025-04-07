module Types.Defs where

data NativeType = I64 | U64 | TyBool
  deriving (Eq)

data TypeInfo = ImmTy | MutTy
  deriving (Eq)

data Type = Native TypeInfo NativeType | TyToInfer | Unit
  deriving (Eq)

instance Show TypeInfo where
  show ImmTy = "immutable"
  show MutTy = "mutable"

instance Show NativeType where
  show I64 = "i64"
  show U64 = "u64"
  show TyBool = "bool"

instance Show Type where
  show (Native ty_info ntype) = show ty_info <> " " <> show ntype
  show TyToInfer = "undefined"
  show Unit = "Unit"

-- | There are cases where we want to cast immutable type to mutable, this function helps
-- with that e.g.
-- x : i64 = 6;
-- y : mut i64 = x + 3;
-- y = y + 1;
sameTypeIgnoreMut :: Type -> Type -> Bool
sameTypeIgnoreMut (Native _ typeInfo) (Native _ typeInfo') = typeInfo == typeInfo'
sameTypeIgnoreMut a b = a == b
