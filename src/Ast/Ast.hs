module Ast.Ast where

import qualified Data.Text as T
import Data.Word

data Func = Func {funcId :: T.Text, funcArgs :: [Expr]}
  deriving (Eq, Show)

data UnaryOp
  = Neg
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  deriving (Eq, Show)

data Expr
  = Const Int
  | ExprCall Func
  | UnaryOp UnaryOp Expr
  | BinOp BinOp Expr Expr
  | Var T.Text
  deriving (Eq, Show)

data Stmt
  = Let T.Text Expr
  | Return Expr
  deriving (Eq, Show)

type Locals = [T.Text]

type StackOffset = Int

data Info = Info {infoLocals :: Locals, infoStackOffset :: StackOffset} deriving (Show, Eq)

data Program = Program {progInfo :: Info, progStmts :: [Stmt]} deriving (Show, Eq)

makeDefaultInfo :: Info
makeDefaultInfo = Info [] 0

addLocal :: T.Text -> Info -> Info
addLocal text (Info locals sOffet) = Info (locals ++ [text]) sOffet
