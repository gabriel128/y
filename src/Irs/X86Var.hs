module Irs.X86Var where

import qualified Ast.Ast as Ast
import qualified Data.Text as T

type Label = T.Text

data Arg = Rax | Imm Int | Var T.Text deriving (Eq, Show)

data BinOp = Add | Sub deriving (Eq, Show)

data Instr
  = BinOp BinOp Arg Arg
  | Neg Arg
  | Mov Arg Arg
  | Call Label
  | Push Arg
  | Pop Arg
  | Ret
  | Jmp Label
  deriving (Eq, Show)

newtype X86Var = X86Var [Instr]

fromAst :: Ast.Program -> Either T.Text X86Var
fromAst (Ast.Program stmt) =
  do
    instrs <- fmap concat (mapM fromStmtToInstrs stmt)
    pure (X86Var instrs)

fromStmtToInstrs :: Ast.Stmt -> Either T.Text [Instr]
fromStmtToInstrs (Ast.Let binding (Ast.Const num)) =
  Right [Mov (Var binding) (Imm num)]
fromStmtToInstrs (Ast.Let binding (Ast.BinOp op expr1 expr2)) = handlBinOp binding op expr1 expr2
fromStmtToInstrs (Ast.Return (Ast.Const num)) = Right [Mov Rax (Imm num), Ret]
fromStmtToInstrs (Ast.Return (Ast.Var binding)) = Right [Mov Rax (Var binding), Ret]
fromStmtToInstrs stmt = Left (T.pack $ "Unhandled stmt: " <> show stmt)

fromBinop :: Ast.BinOp -> BinOp
fromBinop Ast.Add = Add
fromBinop Ast.Sub = Sub

handlBinOp :: T.Text -> Ast.BinOp -> Ast.Expr -> Ast.Expr -> Either T.Text [Instr]
-- x = 2 + 2 -> mov x, 2; add x, 2
handlBinOp x op (Ast.Const num1) (Ast.Const num2) =
  Right [Mov (Var x) (Imm num1), BinOp (fromBinop op) (Var x) (Imm num2)]
-- x = 2 + y -> mov rax, y; add rax, 2; mov x rax
-- x = 2 + x -> add x, 2
handlBinOp x op (Ast.Const num) (Ast.Var y)
  | x == y = Right [Mov Rax (Var y), BinOp (fromBinop op) Rax (Imm num), Mov (Var x) Rax]
  | otherwise = Right [Mov (Var x) (Var y), BinOp (fromBinop op) (Var x) (Imm num)]
-- x = z + y -> mov x, y; add x, z; TODO: Fix with rax
-- x = z + x -> add x, z;
handlBinOp x op (Ast.Var y) (Ast.Var z)
  | x == y = Right [BinOp (fromBinop op) (Var x) (Var z)]
  | otherwise = Right [Mov (Var x) (Var y), BinOp (fromBinop op) (Var x) (Var z)]
handlBinOp _ op exp1 exp2 =
  Left (T.pack $ "Unhandled binOp: " <> show op <> " " <> show exp1 <> " " <> show exp2)
