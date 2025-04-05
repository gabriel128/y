{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import qualified Ast.Ast as Ast
import Control.Monad
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Text (Text, pack, unpack)
import Parser.Defs
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types.Defs (NativeType (I64), Type (..))
import qualified Types.Parsing

runProgramParser :: Text -> Either Text Ast.Program
runProgramParser input =
  case parse parseProgram "" input of
    Left err -> Left (pack $ errorBundlePretty err)
    Right out -> Right out

-- Stmts
parseProgram :: Parser Ast.Program
parseProgram = do
  void spaceConsumer
  stmts <- many parseStmt
  void eof
  return (Ast.Program stmts)

parseStmt :: Parser Ast.Stmt
parseStmt = lexeme $ choice [parsePrint, parseReturn, parseLet]

-- let x : int = 3 + 3;
parseLet :: Parser Ast.Stmt
parseLet = label "let" . lexeme $
  do
    -- void (string "let")
    void space
    var <- parseId
    void (symbol ":")
    ty <- parseTypeId
    void (symbol "=")
    expr <- parseExpr
    void (symbol ";")
    return (Ast.Let ty var expr)

-- return x;
parseReturn :: Parser Ast.Stmt
parseReturn = label "return" . lexeme $
  do
    void (string "return")
    void space1
    expr <- parseExpr
    void (symbol ";")
    return (Ast.Return expr)

parsePrint :: Parser Ast.Stmt
parsePrint = label "print" . lexeme $
  do
    void (string "print")
    expr <- parens parseExpr
    void (symbol ";")
    return (Ast.Print expr)

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
    ],
    [ binary "<<" (Ast.BinOp Ast.ShiftL)
    ]
  ]

binary :: Text -> (Ast.Expr -> Ast.Expr -> Ast.Expr) -> Operator Parser Ast.Expr
binary name f = InfixL (f <$ symbol name)

parseUint :: Parser Ast.Expr
parseUint = Ast.Const I64 <$> lexeme (L.decimal <?> "integer")

parseSignedInt :: Parser Ast.Expr
parseSignedInt = label "signed int" . lexeme $ do
  void (symbol "-")
  Ast.UnaryOp Ast.Neg . Ast.Const I64 <$> L.decimal

parseId :: Parser Text
parseId = label "identifier" . lexeme $ fmap pack $ (:) <$> letterChar <*> many alphaNumChar

parseTypeId :: Parser Type
parseTypeId = label "type identifier" . failsIfError . lexeme $ fmap (Types.Parsing.fromTypeId . pack) $ (:) <$> letterChar <*> many alphaNumChar
  where
    failsIfError :: Parser (Either Text Type) -> Parser Type
    failsIfError parserEither = do
      res <- parserEither
      case res of
        (Right ty) -> return ty
        (Left err) -> fail (unpack err)

parseVar :: Parser Ast.Expr
parseVar = label "var" . lexeme $ fmap (Ast.Var TyToInfer) parseId
