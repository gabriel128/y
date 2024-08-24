module Interpreter.Eval where

import Ast.Ast
import Control.Monad
import Data.Either.Combinators
import Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Prelude as P

type Env = M.Map T.Text Int

data StmtResult = StmtResult {getEnv :: Env, getResult :: Maybe Int} deriving (Show, Eq)

createEnv :: Env
createEnv = empty

--- Interpreter
interpExpr :: Env -> Expr -> Either T.Text Int
interpExpr _ (Const _ty n) = Right n
-- interpExpr env (ExprCall (Func {funcId = "read_input"})) = Right 42
interpExpr env (UnaryOp Neg expr) = fmap (0 -) (interpExpr env expr)
interpExpr env (BinOp Add left right) = (+) <$> interpExpr env left <*> interpExpr env right
interpExpr env (BinOp Sub left right) = (-) <$> interpExpr env left <*> interpExpr env right
interpExpr env (Var _ty binding) = maybeToRight ("Can't find variable " <> binding) (M.lookup binding env)
interpExpr _ expr = Left (T.pack ("Error interpreting expression: " <> show expr))

emptyStmtResult :: StmtResult
emptyStmtResult = StmtResult empty Nothing

interpStmt :: Env -> Stmt -> Either T.Text StmtResult
interpStmt env (Let _ty binding expr) = do
  exprRes <- interpExpr env expr
  let env' = M.insert binding exprRes env
  pure (StmtResult env' Nothing)
interpStmt env (Return expr) = do
  exprRes <- interpExpr env expr
  pure (StmtResult env (Just exprRes))
interpStmt _ _ = Left "wrong statement"

interpStmts :: [Stmt] -> Either T.Text StmtResult
interpStmts = foldM go emptyStmtResult
  where
    go :: StmtResult -> Stmt -> Either T.Text StmtResult
    go (StmtResult env Nothing) stmt = interpStmt env stmt
    go res _ = Right res

interpProg :: Program -> T.Text
interpProg (Program stmts) = either id (maybe "Empty" (T.pack . show) . getResult) (interpStmts stmts)
