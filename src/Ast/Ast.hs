module Ast.Ast where

import Data.Map as M
import qualified Data.Text as T

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

newtype Program = Program [Stmt] deriving (Show)

type Env = M.Map T.Text Int

data StmtResult = StmtResult {getEnv :: Env, getResult :: Maybe Int} deriving (Show)

createEnv :: Env
createEnv = empty
