module Irs.TypedIr where

import Ast.Ast
import Types.Defs

type TypedStmt = Stmt Expr Type
