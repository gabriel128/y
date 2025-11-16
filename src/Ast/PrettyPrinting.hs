{-# OPTIONS_GHC -Wno-orphans #-}

module Ast.PrettyPrinting where

import Ast.Ast
import Data.Text
import Types.Defs
import Utils

instance PrettyPrint Label where
    prettyPrint = tshow

instance PrettyPrint UnaryOp where
    prettyPrint Neg = "-"
    prettyPrint Not = "!"

instance PrettyPrint BinOp where
    prettyPrint Add = " + "
    prettyPrint Sub = " - "
    prettyPrint Mul = " + "
    prettyPrint Div = " \\ "
    prettyPrint Eq = " == "
    prettyPrint Neq = " != "
    prettyPrint Lt = " < "
    prettyPrint Le = " <= "
    prettyPrint And = " && "
    prettyPrint Or = " || "
    prettyPrint ShiftL = " << "

instance (PrettyPrint t) => PrettyPrint (Literal t) where
    prettyPrint (LNum _ num) = tshow num
    prettyPrint (LBool _ bool) = toLower . tshow $ bool
    prettyPrint (LVar _ label) = prettyPrint label

instance (PrettyPrint t) => PrettyPrint (Expr t) where
    prettyPrint (Lit lit) = prettyPrint lit
    prettyPrint (UnaryOp _ op expr) | isAtomicExpr expr = prettyPrint op <> prettyPrint expr
    prettyPrint (UnaryOp _ op expr) = prettyPrint op <> "(" <> prettyPrint expr <> ")"
    prettyPrint (BinOp _ op lh rh) | isAtomicExpr lh && isAtomicExpr rh = prettyPrint lh <> prettyPrint op <> prettyPrint rh
    prettyPrint (BinOp _ op lh rh) | isAtomicExpr lh = prettyPrint lh <> prettyPrint op <> "(" <> prettyPrint rh <> ")"
    prettyPrint (BinOp _ op lh rh) | isAtomicExpr rh = "(" <> prettyPrint lh <> ")" <> prettyPrint op <> prettyPrint rh
    prettyPrint (BinOp _ op lh rh) = "(" <> prettyPrint lh <> ")" <> prettyPrint op <> "(" <> prettyPrint rh <> ")"

instance PrettyPrint (Stmt Expr (Maybe Type)) where
    prettyPrint (Let Nothing label expr) = "let " <> prettyPrint label <> " = " <> prettyPrint expr
    prettyPrint (Let (Just ty) label expr) = "let " <> prettyPrint label <> " : " <> prettyPrint ty <> " = " <> prettyPrint expr
    prettyPrint (Print _ expr) = "print(" <> prettyPrint expr <> ")"
    prettyPrint (Return _ expr) = "return " <> prettyPrint expr

-- instance PrettyPrint (Stmt AExpr) where
--     prettyPrint (Let ty label expr) = "let " <> prettyPrint label <> " : " <> prettyPrint ty <> " = " <> prettyPrint expr
--     prettyPrint (Print _ expr) = "print(" <> prettyPrint expr <> ")"
--     prettyPrint (Return _ expr) = "return " <> prettyPrint expr
