module Parser.Parser where

import qualified Ast.Ast as Ast
import Control.Monad
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Text (Text, pack)
import Parser.Defs
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runProgramParser :: Text -> Either Text Ast.Program
runProgramParser input =
  case parse parseProgram "" input of
    Left err -> Left (pack $ errorBundlePretty err)
    Right out -> Right out

-- Stmts
parseProgram :: Parser Ast.Program
parseProgram = do
  void space
  stmts <- many parseStmt
  void eof
  return (Ast.Program stmts)

parseStmt :: Parser Ast.Stmt
parseStmt = choice [parseLet, parseReturn]

-- let x = 3 + 3;
parseLet :: Parser Ast.Stmt
parseLet = label "let" . lexeme $
  do
    void (string "let")
    void space1
    var <- parseId
    void (symbol "=")
    expr <- parseExpr
    void (symbol ";")
    return (Ast.Let var expr)

-- return x;
parseReturn :: Parser Ast.Stmt
parseReturn = label "return" . lexeme $
  do
    void (string "return")
    void space1
    expr <- parseExpr
    void (symbol ";")
    return (Ast.Return expr)

--- | Exprs
parseTerm :: Parser Ast.Expr
parseTerm = choice [parens parseExpr, parseSignedInt, parseUint, parseVar]

parseExpr :: Parser Ast.Expr
parseExpr = makeExprParser parseTerm opTable

opTable :: [[Operator Parser Ast.Expr]]
opTable =
  [ [ binary "*" (Ast.BinOp Ast.Mul),
      binary "/" (Ast.BinOp Ast.Div)
    ],
    [ binary "+" (Ast.BinOp Ast.Add),
      binary "-" (Ast.BinOp Ast.Sub)
    ]
  ]

binary :: Text -> (Ast.Expr -> Ast.Expr -> Ast.Expr) -> Operator Parser Ast.Expr
binary name f = InfixL (f <$ symbol name)

parseUint :: Parser Ast.Expr
parseUint = Ast.Const <$> lexeme (L.decimal <?> "integer")

parseSignedInt :: Parser Ast.Expr
parseSignedInt = label "signed int" . lexeme $ do
  void (symbol "-")
  num <- L.decimal
  return (Ast.UnaryOp Ast.Neg (Ast.Const num))

parseId :: Parser Text
parseId = label "identifier" . lexeme $ fmap pack $ (:) <$> letterChar <*> many alphaNumChar

parseVar :: Parser Ast.Expr
parseVar = label "var" . lexeme $ fmap Ast.Var parseId
