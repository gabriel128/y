module Ast.Ast where

import Data.Set (Set)
import qualified Data.Set as Set
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
  | UnaryOp UnaryOp Expr
  | BinOp BinOp Expr Expr
  | Var T.Text
  deriving (Eq, Show)

data Stmt
  = Let T.Text Expr
  | Print Expr
  | Return Expr
  deriving (Eq, Show)

-- Locals Vars are meant to be unique.
-- But there is no place in the stack ensured
type Locals = Set T.Text

type StackOffset = Int

data Info = Info {infoLocals :: !Locals, infoStackOffset :: !StackOffset} deriving (Show, Eq)

-- A program is a sequence of statements
newtype Program = Program {progStmts :: [Stmt]} deriving (Show, Eq)

newProgram :: [Stmt] -> Program
newProgram = Program

defaultInfo :: Info
defaultInfo = Info Set.empty 0

addLocal :: T.Text -> Info -> Info
addLocal localVar (Info locals sOffet) = Info (Set.insert localVar locals) sOffet

infoLocalsList :: Info -> [T.Text]
infoLocalsList = Set.toAscList . infoLocals
