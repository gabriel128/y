{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Types.Defs where

data NativeType = I64 | U64 | TyBool | Unit
    deriving (Eq)

data Type where
    MkNativeType :: NativeType -> Type
    TyToInfer :: Type
    deriving (Eq)

instance Show NativeType where
    show I64 = "i64"
    show U64 = "u64"
    show TyBool = "bool"
    show Unit = "Unit"

instance Show Type where
    show (MkNativeType typeval) = show typeval
    show TyToInfer = "not-inferred"
