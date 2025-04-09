{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import Ast.Ast (NativeVal (NativeInt))
import qualified Ast.Ast as Ast
import Control.Monad
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Text (Text, pack, unpack)
import Parser.Defs
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types.Defs (NativeType (I64), Type (..), TypeInfo (..))
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
parseStmt = lexeme $ choice [block parseStmt, parsePrint, parseReturn, try parseLet <|> parseLetToInfer]

-- x : int = 3 + 3;
parseLet :: Parser Ast.Stmt
parseLet = label "let" . lexeme $
  do
    void space
    var <- parseId
    void (symbol ":")
    typeInfo <- parseTypeInfo
    ty <- parseTypeId typeInfo
    void (symbol "=")
    expr <- parseExpr
    void (symbol ";")
    return $ Ast.Let ty var expr

parseTypeInfo :: Parser TypeInfo
parseTypeInfo = label "let" . lexeme $
  do
    void space
    typeInfo <- option "" (symbol "mut")
    return (if typeInfo == "mut" then MutTy else ImmTy)

-- x = 3 + 3;
parseLetToInfer :: Parser Ast.Stmt
parseLetToInfer = label "let inferred" . lexeme $
  do
    void space
    var <- parseId
    void (symbol "=")
    expr <- parseExpr
    void (symbol ";")
    return $ Ast.Let TyToInfer var expr

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
parseTerm = choice [parens parseExpr, try parseSignedInt <|> parseNegation, parseUint, parseVar]

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
parseUint = Ast.Const I64 . NativeInt <$> lexeme (L.decimal <?> "integer")

-- parseBool :: Parser Ast.Expr
-- parseBool = Ast.Const TyBool $ NativeBool <$> lexeme (L.decimal <?> "integer")

parseSignedInt :: Parser Ast.Expr
parseSignedInt = label "signed int" . lexeme $ do
  void (symbol "-")
  Ast.UnaryOp Ast.Neg . Ast.Const I64 . NativeInt <$> L.decimal

parseNegation :: Parser Ast.Expr
parseNegation = label "signed int" . lexeme $ do
  void (symbol "-")
  Ast.UnaryOp Ast.Neg <$> parseExpr

parseId :: Parser Text
parseId = label "identifier" . lexeme $ do
  firstLetter <- letterChar
  rest <- many (alphaNumChar <|> char '-' <|> char '_')
  pure $ pack (firstLetter : rest)

parseTypeId :: TypeInfo -> Parser Type
parseTypeId typeInfo = label "type identifier" . failsIfError . lexeme $ fmap (Types.Parsing.fromTypeId typeInfo . pack) $ (:) <$> letterChar <*> many alphaNumChar
  where
    failsIfError :: Parser (Either Text Type) -> Parser Type
    failsIfError parserEither = do
      res <- parserEither
      case res of
        (Right ty) -> return ty
        (Left err) -> fail (unpack err)

parseVar :: Parser Ast.Expr
parseVar = label "var" . lexeme $ fmap (Ast.Var TyToInfer) parseId

-- Experiments

-- parseLetModifier :: Parser Text
-- parseLetModifier = choice [string "const", string "mut"]
