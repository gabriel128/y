import AtomizeTests
import FullProgramTests (test_full_progs)
import InterpreterTests
import ParserTests (test_parser)
import Test.DocTest (mainFromCabal)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  mainFromCabal "yacll" []
  -- doctests
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [test_programs, test_id_ir, test_parser, test_full_progs]
