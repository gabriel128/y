{-# LANGUAGE QuasiQuotes #-}

module Irs.X86ToTextProg where

import Ast.Ast (Info (infoStackOffset))
import Control.Carrier.State.Strict
import Data.List (intersperse)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Irs.PassEffs as PassEffs
import qualified Nasm.Data as Nasm
import NeatInterpolation
import Utils

instrsToText :: [Nasm.Instr] -> PassEffs.StErr sig m Text
instrsToText intrs = do
  stackOffset <- gets infoStackOffset
  let textInstrs = printInstrs intrs
  pure (programTemplate textInstrs stackOffset)

printInstrs :: [Nasm.Instr] -> Text
printInstrs instrs = T.concat $ intersperse " \n" (fmap textPrint instrs)

programTemplate :: Text -> Int -> Text
programTemplate logic stackOffset =
  let offset = pack $ show stackOffset
   in [trimming|
        .global main
      main:
          pushq   rbp
          movq    rbp, rsp
          subq    rsp, $offset
          jmp start

      start:
        $logic
        jmp conclusion

      conclusion:
          addq    rsp, rbp
          popq    rbp
          ret
    |]
