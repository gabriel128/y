{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import Ast.Ast
import qualified Ast.Ast as Ast
import Control.Monad
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Text (Text, pack)
import Parser.Defs
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types.Defs
import Types.Parsing (parseTypeId)

runProgramParser :: Text -> Either Text (Program Expr (Maybe Type))
runProgramParser input =
    case parse parseProgram "" input of
        Left err -> Left (pack $ errorBundlePretty err)
        Right out -> Right out

-- Stmts
parseProgram :: Parser (Program Expr (Maybe Type))
parseProgram = do
    void spaceConsumer
    stmts <- many parseStmt
    void eof
    return (Program stmts)

parseStmt :: Parser (Stmt Expr (Maybe Type))
parseStmt = lexeme $ choice [block parseStmt, parsePrint, parseReturn, try parseLet <|> parseLetToInfer]

-- x : int = 3 + 3;
parseLet :: Parser (Stmt Expr (Maybe Type))
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
parseLetToInfer :: Parser (Stmt Expr (Maybe Type))
parseLetToInfer = label "let infer" . lexeme $
    do
        void space
        var <- parseId
        void (symbol "=")
        expr <- parseExpr
        void (symbol ";")
        return $ Let Nothing var expr

-- return x;
parseReturn :: Parser (Stmt Expr (Maybe Type))
parseReturn = label "return" . lexeme $
    do
        void (string "return")
        void space1
        expr <- parseExpr
        void (symbol ";")
        return . Return Nothing $ expr

parsePrint :: Parser (Stmt Expr (Maybe Type))
parsePrint = label "print" . lexeme $
    do
        void (string "print")
        expr <- parens parseExpr
        void (symbol ";")
        return $ Print Unit expr

--- | Exprs
parseTerm :: Parser (Expr (Maybe Type))
parseTerm = choice [parens parseExpr, try parseSignedInt <|> parseNegation, parseInt, parseVar]

parseExpr :: Parser (Expr (Maybe Type))
parseExpr = makeExprParser parseTerm opTable

-- The outer list is ordered in descending precedence, so the higher we place a group of operators in it, the tighter they bind
-- Roughly based on this https://en.cppreference.com/w/c/language/operator_precedence.html
opTable :: [[Operator Parser (Expr (Maybe Type))]]
opTable =
    [ [prefix "!" (UnaryOp (Just TyBool) Ast.Not)]
    ,
        [ binary "*" (BinOp Nothing Ast.Mul)
        , binary "/" (BinOp Nothing Ast.Div)
        ]
    ,
        [ binary "+" (BinOp Nothing Ast.Add)
        , binary "-" (BinOp Nothing Ast.Sub)
        ]
    ,
        [ binary "<<" (BinOp Nothing Ast.ShiftL)
      -- binary ">>" (BinOp Nothing Ast.ShiftR)
        ]
    ,
        [ binary "<=" (BinOp (Just TyBool) Ast.Le)
        , binaryFlipped ">=" (BinOp (Just TyBool) Ast.Le)
        , binary "<" (BinOp (Just TyBool) Ast.Lt)
        , binaryFlipped ">" (BinOp (Just TyBool) Ast.Lt)
        ]
    , -- [ binary "&" (BinOp Nothing Ast.BinOp.BitAnd) ],
      -- [ binary "|" (BinOp Nothing Ast.BinOp.BitOr) ],
      -- [ binary "^" (BinOp Nothing Ast.BinOp.BitXor) ],
      -- [ binary "&&" (BinOp Nothing Ast.BinOp.And) ],
      -- [ binary "||" (BinOp Nothing Ast.BinOp.Or) ],

        [ binary "==" (BinOp (Just TyBool) Ast.Eq)
      -- binary "!=" (BinOp (mkImmNativeType TyBool) Ast.BinOp.Neq),
        ]
    ]

prefix :: Text -> (Expr (Maybe Type) -> Expr (Maybe Type)) -> Operator Parser (Expr (Maybe Type))
prefix name f = Prefix (f <$ symbol name)

binary :: Text -> (Expr (Maybe Type) -> Expr (Maybe Type) -> Expr (Maybe Type)) -> Operator Parser (Expr (Maybe Type))
binary name f = InfixL (f <$ symbol name)

binaryFlipped :: Text -> (Expr (Maybe Type) -> Expr (Maybe Type) -> Expr (Maybe Type)) -> Operator Parser (Expr (Maybe Type))
binaryFlipped name f = InfixL (flip f <$ symbol name)

parseUint :: Parser (Expr (Maybe Type))
parseUint = Lit . LNum U64 <$> lexeme (L.decimal <?> "integer")

parseInt :: Parser (Expr (Maybe Type))
parseInt = Lit . LNum I64 <$> lexeme (L.decimal <?> "integer")

-- parseBool :: Parser Ast.(Expr (Maybe Type))
-- parseBool = Ast.Const TyBool $ NativeBool <$> lexeme (L.decimal <?> "integer")

-- TODO: Make this a prefix op
parseSignedInt :: Parser (Expr (Maybe Type))
parseSignedInt = label "signed int" . lexeme $ do
    void (symbol "-")
    UnaryOp (Just I64) Ast.Neg . Lit . LNum I64 <$> L.decimal

parseNegation :: Parser (Expr (Maybe Type))
parseNegation = label "signed int" . lexeme $ do
    void (symbol "-")
    UnaryOp Nothing Ast.Neg <$> parseExpr

parseId :: Parser Text
parseId = label "identifier" . lexeme $ do
    firstLetter <- letterChar
    rest <- many (alphaNumChar <|> char '-' <|> char '_')
    pure $ pack (firstLetter : rest)

parseVar :: Parser (Expr (Maybe Type))
parseVar = label "var" . lexeme $ fmap (Lit . LVar Nothing) parseId

-- Experiments

-- parseLetModifier :: Parser Text
-- parseLetModifier = choice [string "const", string "mut"]
