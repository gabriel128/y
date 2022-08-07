module Lvar where

import Control.Monad
import Data.Either.Combinators
import Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Utils
import Prelude as P

-- exp ::= Int
--     | (UnaryOp
--     | (Prim '- (list exp))
--     | (Prim '+ (list exp exp))
-- Program ::= Program {[] [exp]})

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

interpExpr :: Env -> Expr -> Either T.Text Int
interpExpr _ (Const n) = Right n
-- interpExpr env (ExprCall (Func {funcId = "read_input"})) = Right 42
interpExpr env (UnaryOp Neg expr) = fmap (0 -) (interpExpr env expr)
interpExpr env (BinOp Add left right) = (+) <$> interpExpr env left <*> interpExpr env right
interpExpr env (BinOp Sub left right) = (-) <$> interpExpr env left <*> interpExpr env right
interpExpr env (Var binding) = maybeToRight ("Can't find variable " <> binding) (M.lookup binding env)
interpExpr _ expr = Left (T.pack ("Error interpreting expression: " <> show expr))

emptyStmtResult :: StmtResult
emptyStmtResult = StmtResult empty Nothing

interpStmt :: Env -> Stmt -> Either T.Text StmtResult
interpStmt env (Let binding expr) = do
  exprRes <- interpExpr env expr
  let env' = M.insert binding exprRes env
  pure (StmtResult env' Nothing)
interpStmt env (Return expr) = do
  exprRes <- interpExpr env expr
  pure (StmtResult env (Just exprRes))

removeComplexExp :: Expr -> [(Stmt, Expr)]
removeComplexExp (Const _) = []
removeComplexExp (Var _) = []
removeComplexExp expr = let varName = Utils.randVarName in [(Let varName expr, Var varName)]

removeComplexStmt :: Stmt -> [Stmt]
removeComplexStmt initStmt@(Let binding expr) = go (removeComplexExp expr)
  where
    go [(newStmt, expr')] = [newStmt, Let binding expr']
    go _ = [initStmt]

-- Makes the bound variables have a unique_name
interpStmts :: [Stmt] -> Either T.Text StmtResult
interpStmts = foldM go emptyStmtResult
  where
    go :: StmtResult -> Stmt -> Either T.Text StmtResult
    go (StmtResult env Nothing) stmt = interpStmt env stmt
    go res _ = Right res

interpProg :: Program -> T.Text
interpProg (Program stmts) = either id (maybe "Empty" (T.pack . show) . getResult) (interpStmts stmts)

-- Test Programs
stmt0 :: Stmt
stmt0 = Let "x" (Const 10)

stmt1 :: Stmt
stmt1 = Let "y" (Var "x")

stmt2 :: Stmt
stmt2 = Return (BinOp Add (Var "y") (UnaryOp Neg (Const 8)))

stmt3 :: Stmt
stmt3 = Return (BinOp Add (Var "z") (UnaryOp Neg (Const 7)))

testProg :: T.Text
testProg = interpProg (Program [stmt0, stmt1, stmt2, stmt3])

-- Not for now

-- interpretStmt :: Stmt -> ExceptT String IO ()
-- interpretStmt (StmtCall (Func {funcId = "print", funcArgs = [expr]})) = lift . print $ interpretExpr expr
-- interpretStmt _ = throwError "Error interpreting statement"

-- interpretModule :: Module -> IO [Either String ()]
-- interpretModule Module {moduleBody = body} = traverse (runExceptT . interpretStmt) body

-- interpStmt :: Stmt -> Either String (IO ())
-- interpStmt (StmtCall (Func {funcId = "print", funcArgs = [expr]})) = fmap print (interpExpr expr)
-- interpStmt _ = Left "Error"

-- data Module = Module [Stmt] deriving (Show)

-- -- interpModule :: Module -> IO [Either String ()]
-- interpModule :: Module -> IO ()
-- interpModule (Module body) =
--   case traverse interpStmt body of
--     (Left error) -> print error
--     (Right ioOp) -> sequence_ ioOp

-- testProg1 =
--   interpModule (Module [StmtCall (Func {funcId = "print", funcArgs = [Const 8]})])
