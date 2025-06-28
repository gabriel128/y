{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import Ast.Ast
import qualified Ast.Ast as Ast
import qualified Ast.Ast as Ast.BinOp
import Control.Monad
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Parser.Defs
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types.Defs
import Types.Parsing (parseTypeId)
import qualified Types.Parsing
import Utils (tshow)

runProgramParser :: Text -> Either Text Program
runProgramParser input =
    case parse parseProgram "" input of
        Left err -> Left (pack $ errorBundlePretty err)
        Right out -> Right out

-- Stmts
parseProgram :: Parser Program
parseProgram = do
    void spaceConsumer
    stmts <- many parseStmt
    void eof
    return (Program stmts)

parseStmt :: Parser Stmt
parseStmt = lexeme $ choice [block parseStmt, parsePrint, parseReturn, try parseLet <|> parseLetToInfer]

-- x : int = 3 + 3;
parseLet :: Parser Stmt
parseLet = label "assignment" . lexeme $
    do
        void space
        var <- parseId
        void (symbol ":")
        ty <- parseTypeId
        void (symbol "=")
        expr <- parseExpr
        void (symbol ";")
        return $ Let ty var expr

-- x = 3 + 3;
parseLetToInfer :: Parser Stmt
parseLetToInfer = label "let infer" . lexeme $
    do
        void space
        var <- parseId
        void (symbol "=")
        expr <- parseExpr
        void (symbol ";")
        return $ Let TyToInfer var expr

-- return x;
parseReturn :: Parser Stmt
parseReturn = label "return" . lexeme $
    do
        void (string "return")
        void space1
        expr <- parseExpr
        void (symbol ";")
        return . Return TyToInfer $ expr

parsePrint :: Parser Stmt
parsePrint = label "print" . lexeme $
    do
        void (string "print")
        expr <- parens parseExpr
        void (symbol ";")
        return $ Print (MkNativeType Unit) expr

--- | Exprs
parseTerm :: Parser Expr
parseTerm = choice [parens parseExpr, try parseSignedInt <|> parseNegation, parseInt, parseVar]

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm opTable

-- The outer list is ordered in descending precedence, so the higher we place a group of operators in it, the tighter they bind
-- Roughly based on this https://en.cppreference.com/w/c/language/operator_precedence.html
opTable :: [[Operator Parser Expr]]
opTable =
    [ [prefix "!" (UnaryOp (MkNativeType TyBool) Ast.Not)]
    ,
        [ binary "*" (BinOp TyToInfer Ast.Mul)
        , binary "/" (BinOp TyToInfer Ast.Div)
        ]
    ,
        [ binary "+" (BinOp TyToInfer Ast.Add)
        , binary "-" (BinOp TyToInfer Ast.Sub)
        ]
    ,
        [ binary "<<" (BinOp TyToInfer Ast.ShiftL)
      -- binary ">>" (BinOp TyToInfer Ast.ShiftR)
        ]
    ,
        [ binary "<=" (BinOp (MkNativeType TyBool) Ast.Le)
        , binaryFlipped ">=" (BinOp (MkNativeType TyBool) Ast.Le)
        , binary "<" (BinOp (MkNativeType TyBool) Ast.Lt)
        , binaryFlipped ">" (BinOp (MkNativeType TyBool) Ast.Lt)
        ]
    , -- [ binary "&" (BinOp TyToInfer Ast.BinOp.BitAnd) ],
      -- [ binary "|" (BinOp TyToInfer Ast.BinOp.BitOr) ],
      -- [ binary "^" (BinOp TyToInfer Ast.BinOp.BitXor) ],
      -- [ binary "&&" (BinOp TyToInfer Ast.BinOp.And) ],
      -- [ binary "||" (BinOp TyToInfer Ast.BinOp.Or) ],

        [ binary "==" (BinOp (MkNativeType TyBool) Ast.Eq)
      -- binary "!=" (BinOp (mkImmNativeType TyBool) Ast.BinOp.Neq),
        ]
    ]

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

binaryFlipped :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryFlipped name f = InfixL (flip f <$ symbol name)

parseUint :: Parser Expr
parseUint = Const U64 . tshow <$> lexeme (L.decimal <?> "integer")

parseInt :: Parser Expr
parseInt = Const I64 . tshow <$> lexeme (L.decimal <?> "integer")

-- parseBool :: Parser Ast.Expr
-- parseBool = Ast.Const TyBool $ NativeBool <$> lexeme (L.decimal <?> "integer")

-- TODO: Make this a prefix op
parseSignedInt :: Parser Expr
parseSignedInt = label "signed int" . lexeme $ do
    void (symbol "-")
    UnaryOp (MkNativeType I64) Ast.Neg . Const I64 . tshow <$> L.decimal

parseNegation :: Parser Expr
parseNegation = label "signed int" . lexeme $ do
    void (symbol "-")
    UnaryOp TyToInfer Ast.Neg <$> parseExpr

parseId :: Parser Text
parseId = label "identifier" . lexeme $ do
    firstLetter <- letterChar
    rest <- many (alphaNumChar <|> char '-' <|> char '_')
    pure $ pack (firstLetter : rest)

parseVar :: Parser Expr
parseVar = label "var" . lexeme $ fmap (Var TyToInfer) parseId

-- Experiments

-- parseLetModifier :: Parser Text
-- parseLetModifier = choice [string "const", string "mut"]
