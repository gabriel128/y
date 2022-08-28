{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Represents type checked X86 in NASM format
module Irs.X86 where

-- import qualified Ast.Ast as Ast
-- import Data.Either.Combinators (maybeToRight)
-- import qualified Data.Map as M
-- import qualified Data.Text as T
-- import Irs.X86Var (X86Var)
-- import qualified Irs.X86Var as X86Var

-- data X86Program = X86Program Ast.Info [Instr]

-- newtype LocalStackMap = LocalStackMap (M.Map T.Text MemDeref)

-- fromX86Var :: X86Var -> Either T.Text X86Program
-- fromX86Var (X86Var.X86Var info varInstrs) =
--   let (stackOffset, instrs) = generateInstrs info varInstrs
--    in X86Program <$> pure (info {Ast.infoStackOffset = stackOffset}) <*> instrs

-- generateInstrs :: Ast.Info -> [X86Var.Instr] -> (Offset, Either T.Text [Instr])
-- generateInstrs info instrs =
--   let locals = Ast.infoLocals $ info
--       (stackPosition, localsStackMapping) = mapLocalsToStack locals
--       nasmInstrs = fmap (convertTo localsStackMapping) instrs
--    in (stackPosition, undefined)

-- -- [ebp - n]
-- mapLocalsToStack :: [T.Text] -> (Offset, LocalStackMap)
-- mapLocalsToStack = foldr reducer (0, LocalStackMap M.empty)
--   where
--     reducer :: T.Text -> (Offset, LocalStackMap) -> (Offset, LocalStackMap)
--     reducer local (n, LocalStackMap amap) =
--       let amap' = M.insert local (MemDeref Rbp (n - 8)) amap
--        in (n - 8, LocalStackMap amap')

-- -- There is a 1:1 correspondence between X86Var.Instr and Instr
-- convertTo :: LocalStackMap -> X86Var.Instr -> Either T.Text [Instr]
-- -- mov x 3 -> mov [rbp - n] 3
-- convertTo (LocalStackMap mapping) (X86Var.Mov (X86Var.Var local) (X86Var.Imm num)) =
--   let memRef = maybeToRight ("Local not mapped: " <> local) (M.lookup local mapping)
--    in fmap (\memRef' -> [mov (MI memRef' (Imm num))]) memRef
-- convertTo _ (X86Var.Mov X86Var.Rax (X86Var.Imm num)) = Right [mov (RI Rax (Imm num))]
-- convertTo _ instr = Left (T.pack $ "X86Var Instruction not handled: " <> show instr)
