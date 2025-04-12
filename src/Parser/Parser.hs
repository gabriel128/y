{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import Ast.Ast (NativeVal (NativeInt))
import qualified Ast.Ast as Ast
import Ast.TypedAst (TypedExpr (..), TypedProgram (..), TypedStmt (..))
import Control.Monad
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Text (Text, pack, unpack)
import Parser.Defs
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types.Defs
import qualified Types.Parsing

runProgramParser :: Text -> Either Text TypedProgram
runProgramParser input =
  case parse parseProgram "" input of
    Left err -> Left (pack $ errorBundlePretty err)
    Right out -> Right out

-- Stmts
parseProgram :: Parser TypedProgram
parseProgram = do
  void spaceConsumer
  stmts <- many parseStmt
  void eof
  return (TypedProgram stmts)

parseStmt :: Parser TypedStmt
parseStmt = lexeme $ choice [block parseStmt, parsePrint, parseReturn, try parseLet <|> parseLetToInfer]

-- x : int = 3 + 3;
parseLet :: Parser TypedStmt
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
    return $ TLet ty var expr

parseTypeInfo :: Parser TypeMeta
parseTypeInfo = label "let" . lexeme $
  do
    void space
    typeInfo <- option "" (symbol "mut")
    return (if typeInfo == "mut" then TypeMeta True else TypeMeta False)

-- x = 3 + 3;
parseLetToInfer :: Parser TypedStmt
parseLetToInfer = label "let inferred" . lexeme $
  do
    void space
    var <- parseId
    void (symbol "=")
    expr <- parseExpr
    void (symbol ";")
    return $ TLet TyToInfer var expr

-- return x;
parseReturn :: Parser TypedStmt
parseReturn = label "return" . lexeme $
  do
    void (string "return")
    void space1
    expr <- parseExpr
    void (symbol ";")
    return . TReturn TyToInfer $ expr

parsePrint :: Parser TypedStmt
parsePrint = label "print" . lexeme $
  do
    void (string "print")
    expr <- parens parseExpr
    void (symbol ";")
    return $ TPrint (mkImmNativeType Unit) expr

--- | Exprs
parseTerm :: Parser TypedExpr
parseTerm = choice [parens parseExpr, try parseSignedInt <|> parseNegation, parseUint, parseVar]

parseExpr :: Parser TypedExpr
parseExpr = makeExprParser parseTerm opTable

opTable :: [[Operator Parser TypedExpr]]
opTable =
  [ [ binary "*" (TBinOp TyToInfer Ast.Mul),
      binary "/" (TBinOp TyToInfer Ast.Div)
    ],
    [ binary "+" (TBinOp TyToInfer Ast.Add),
      binary "-" (TBinOp TyToInfer Ast.Sub)
    ],
    [ binary "<<" (TBinOp TyToInfer Ast.ShiftL)
    ]
  ]

binary :: Text -> (TypedExpr -> TypedExpr -> TypedExpr) -> Operator Parser TypedExpr
binary name f = InfixL (f <$ symbol name)

parseUint :: Parser TypedExpr
parseUint = TConst (mkImmNativeType U64) . NativeInt <$> lexeme (L.decimal <?> "integer")

-- parseBool :: Parser Ast.Expr
-- parseBool = Ast.Const TyBool $ NativeBool <$> lexeme (L.decimal <?> "integer")

parseSignedInt :: Parser TypedExpr
parseSignedInt = label "signed int" . lexeme $ do
  void (symbol "-")
  TUnaryOp (mkImmNativeType I64) Ast.Neg . TConst (mkImmNativeType I64) . NativeInt <$> L.decimal

parseNegation :: Parser TypedExpr
parseNegation = label "signed int" . lexeme $ do
  void (symbol "-")
  TUnaryOp TyToInfer Ast.Neg <$> parseExpr

parseId :: Parser Text
parseId = label "identifier" . lexeme $ do
  firstLetter <- letterChar
  rest <- many (alphaNumChar <|> char '-' <|> char '_')
  pure $ pack (firstLetter : rest)

parseTypeId :: TypeMeta -> Parser Type
parseTypeId typeInfo = label "type identifier" . failsIfError . lexeme $ fmap (Types.Parsing.fromTypeId typeInfo . pack) $ (:) <$> letterChar <*> many alphaNumChar
  where
    failsIfError :: Parser (Either Text Type) -> Parser Type
    failsIfError parserEither = do
      res <- parserEither
      case res of
        (Right ty) -> return ty
        (Left err) -> fail (unpack err)

parseVar :: Parser TypedExpr
parseVar = label "var" . lexeme $ fmap (TVar TyToInfer) parseId

-- Experiments

-- parseLetModifier :: Parser Text
-- parseLetModifier = choice [string "const", string "mut"]
