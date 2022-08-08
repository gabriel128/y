module Irs.Id where

import Ast.Ast
import Utils

removeComplexExp :: Expr -> [(Stmt, Expr)]
removeComplexExp (Const _) = []
removeComplexExp (Var _) = []
removeComplexExp expr = let varName = Utils.randVarName in [(Let varName expr, Var varName)]

removeComplexStmt :: Stmt -> [Stmt]
removeComplexStmt initStmt@(Let binding expr) = go (removeComplexExp expr)
  where
    go [(newStmt, expr')] = [newStmt, Let binding expr']
    go _ = [initStmt]
removeComplexStmt stmt = [stmt]
