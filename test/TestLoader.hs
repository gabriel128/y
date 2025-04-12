import AtomizeTests
import DsTests
import FullProgramTests (test_full_progs)
import NasmTests (test_nasm)
import ParserTests (test_parser)
import RegisterAllocTests (test_reg_alloc)
import Test.DocTest (mainFromCabal)
import Test.Tasty (TestTree, defaultMain, testGroup)
import TypeCheckerTests

main :: IO ()
main = do
  mainFromCabal "y" []
  -- doctests
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [test_id_ir, test_parser, test_full_progs, test_reg_alloc, test_general_ds, dsProps, test_nasm, test_type_checking]
