{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Types.Defs where

import Utils (PrettyPrint (..))

data NativeType = I64 | U64 | TyBool | Unit
    deriving (Show, Eq)

data Type where
    MkNativeType :: NativeType -> Type
    TyToInfer :: Type
    deriving (Show, Eq)

instance PrettyPrint NativeType where
    prettyPrint I64 = "i64"
    prettyPrint U64 = "u64"
    prettyPrint TyBool = "bool"
    prettyPrint Unit = "Unit"

instance PrettyPrint Type where
    prettyPrint (MkNativeType typeval) = prettyPrint typeval
    prettyPrint TyToInfer = "ToInfer"

isNumeric :: NativeType -> Bool
isNumeric I64 = True
isNumeric U64 = True
isNumeric _ = False
