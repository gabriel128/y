{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Types.Defs where

import Utils (PrettyPrint (..))

data Type = I64 | U64 | TyBool | Unit
    deriving (Show, Eq)

instance PrettyPrint Type where
    prettyPrint I64 = "i64"
    prettyPrint U64 = "u64"
    prettyPrint TyBool = "bool"
    prettyPrint Unit = "Unit"

instance PrettyPrint (Maybe Type) where
    prettyPrint (Just typeval) = prettyPrint typeval
    prettyPrint Nothing = "ToInfer"

isNumeric :: Type -> Bool
isNumeric I64 = True
isNumeric U64 = True
isNumeric _ = False
