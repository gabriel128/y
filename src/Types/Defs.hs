{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Types.Defs where

data NativeType = I64 | U64 | TyBool | Unit
  deriving (Eq)

-- data SomeType = forall a. SomeType (Type a)

data Type where
  TyNative :: TypeMeta -> NativeType -> Type
  TyToInfer :: Type
  deriving (Eq)

newtype TypeMeta = TypeMeta {mut :: Bool}
  deriving (Eq)

mkImmNativeType :: NativeType -> Type
mkImmNativeType = TyNative TypeMeta {mut = False}

mkMutNativeType :: NativeType -> Type
mkMutNativeType = TyNative TypeMeta {mut = True}

instance Show TypeMeta where
  show TypeMeta {mut = True} = "mut"
  show TypeMeta {mut = False} = "immut"

instance Show NativeType where
  show I64 = "i64"
  show U64 = "u64"
  show TyBool = "bool"
  show Unit = "Unit"

instance Show Type where
  show (TyNative ty_info typeval) = show ty_info <> " " <> show typeval
  show TyToInfer = "undefined"

-- | There are cases where we want to cast immutable type to mutable, this function helps
-- with that e.g.
-- x : i64 = 6;
-- y : mut i64 = x + 3;
-- y = y + 1;
sameTypeIgnoreMut :: Type -> Type -> Bool
sameTypeIgnoreMut (TyNative _ typeval) (TyNative _ typeval') = typeval == typeval'
sameTypeIgnoreMut a b = a == b
