module Ast.Ast where

import qualified Data.Text as T

data Func = Func {funcId :: T.Text, funcArgs :: [Expr]}
  deriving (Eq, Show)

data UnaryOp
  = Neg
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | ShiftL
  deriving (Eq, Show)

data Expr
  = Const Int
  | Fn Func
  | UnaryOp UnaryOp Expr
  | BinOp BinOp Expr Expr
  | Var T.Text
  deriving (Eq, Show)

data Stmt
  = Let T.Text Expr
  | Print Expr
  | Return Expr
  deriving (Eq, Show)

type Locals = [T.Text]

type StackOffset = Int

data Info = Info {infoLocals :: !Locals, infoStackOffset :: !StackOffset} deriving (Show, Eq)

-- A program is a sequence of statements
newtype Program = Program {progStmts :: [Stmt]} deriving (Show, Eq)

newProgram :: [Stmt] -> Program
newProgram = Program

defaultInfo :: Info
defaultInfo = Info [] 0

addLocal :: T.Text -> Info -> Info
addLocal localVar (Info locals sOffet) | localVar `elem` locals = Info locals sOffet
addLocal localVar (Info locals sOffet) = Info (locals ++ [localVar]) sOffet
