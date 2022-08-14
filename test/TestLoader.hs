import AtomizeTests
import InterpreterTests
import ParserTests (test_parser)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [test_programs, test_id_ir, test_parser]
