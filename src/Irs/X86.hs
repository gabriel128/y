{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Represents type checked X86 in NASM format
module Irs.X86 where

import Ast.Ast
import qualified Ast.Ast as Ast
-- import Data.Either.Combinators (maybeToRight)

import Control.Carrier.Error.Either
import Control.Carrier.State.Strict
import Data.Either.Combinators (maybeToRight)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Nasm.Data
import Nasm.Dsl

-- data X86Program = X86Program Ast.Info [Instr]

type LocalStackMap = M.Map Text MemDeref

lookupEither :: Text -> LocalStackMap -> Either Text MemDeref
lookupEither binding mapping = maybeToRight ("Local not mapped: " <> binding) (M.lookup binding mapping)

mapLocalsToStack :: [Text] -> (Offset, LocalStackMap)
mapLocalsToStack = foldr reducer (0, M.empty)
  where
    reducer :: Text -> (Offset, LocalStackMap) -> (Offset, LocalStackMap)
    reducer local (n, amap) =
      let amap' = M.insert local (MemDeref Rbp (n - 8)) amap
       in (n - 8, amap')

getStackMapping :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => Text -> m MemDeref
getStackMapping binding = gets (lookupEither binding) >>= liftEither >>= pure

fromStmtToInstrs :: (Has (State LocalStackMap) sig m, (Has (Throw Text) sig m)) => Stmt -> m [Instr]
-- x = 3;
fromStmtToInstrs (Let binding (Const num)) = do
  x <- getStackMapping binding
  pure [movmi x num]
-- return 4;
fromStmtToInstrs (Return (Const num)) =
  pure [movri Rax num, ret]
-- return x;
fromStmtToInstrs (Return (Var binding)) =
  getStackMapping binding >>= (\x -> pure [movrm Rax x, ret])
-- Handle addition
-- x = 2 + 2; -> mov x, 2; add x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num1) (Const num2))) = do
  x <- getStackMapping binding
  pure [movmi x num1, addmi x num2]
-- x = 2 + y; -> mov rax, y; add rax, 2; mov x rax
-- x = 2 + x; -> add x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  if binding == binding2
    then pure [addmi x 2]
    else pure [movrm Rax y, addri Rax num, movmr x Rax]
-- Add is commutative so we just call the above definition
fromStmtToInstrs (Let binding (BinOp Ast.Add (Var binding2) (Const num))) =
  fromStmtToInstrs (Let binding (BinOp Ast.Add (Const num) (Var binding2)))
-- x = z + y; -> mov rax, z; add rax, y; mov x, rax
fromStmtToInstrs (Let binding (BinOp Ast.Add (Var binding1) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding1
  z <- getStackMapping binding2
  pure [movrm Rax z, addrm Rax y, movmr x Rax]

-- Handle substaction
-- x = 2 - 2 -> mov x, 2; sub x, 2
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Const num1) (Const num2))) = do
  x <- getStackMapping binding
  pure [movmi x num1, submi x num2]
-- x = 2 - y -> mov rax, 2; sub rax, y; mov x rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Const num) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [movri Rax num, subrm Rax y, movmr x Rax]
-- x = y - 2 -> mov rax, y; sub rax, 2; mov x rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Var binding2) (Const num))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding2
  pure [movrm Rax y, subri Rax num, movmr x Rax]
-- x = z - y -> mov rax, z; sub rax, y; mov x, rax
fromStmtToInstrs (Let binding (BinOp Ast.Sub (Var binding1) (Var binding2))) = do
  x <- getStackMapping binding
  y <- getStackMapping binding1
  z <- getStackMapping binding2
  pure [movrm Rax z, subrm Rax y, movmr x Rax]

-- Unhandled
fromStmtToInstrs stmt = throwError (pack $ "Unhandled stmt: " <> show stmt)
