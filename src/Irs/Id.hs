module Irs.Id where

import Ast.Ast
import Utils

removeComplexExp :: Expr -> [(Stmt, Expr)]
removeComplexExp (Const _) = []
removeComplexExp (Var _) = []
removeComplexExp expr =
  let varName = Utils.randVarName
   in [(Let varName expr, Var varName)]

-- TODO: Handle the recursive casees
-- let x = (1 + (2 + (3 + 4)))
-- =>
-- let y = (2 + (3 + 4)); let x = 1 + y
-- =>
-- let z = 3 + 4; let y = 2 + z; let x = 1 + y

removeComplexStmt :: Stmt -> [Stmt]
removeComplexStmt initStmt@(Let binding expr) = go (removeComplexExp expr)
  where
    go [(newStmt, expr')] = [newStmt, Let binding expr']
    go _ = [initStmt]
removeComplexStmt stmt = [stmt]
