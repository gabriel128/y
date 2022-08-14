module Parser.Parser where

import qualified Ast.Ast as Ast
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- Assume no white space before token and consume all white space after token”
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parseInt :: Parser Ast.Expr
parseInt = Ast.Const <$> lexeme (L.decimal <?> "integer")

parseSignedInt :: Parser Ast.Expr
parseSignedInt = do
  void (symbol "-")
  num <- L.decimal
  return (Ast.UnaryOp Ast.Neg (Ast.Const num))

-- (1)
-- parseLet :: Parsec String st String
-- parseLet =
--   string "let"
--     <*> many space
--     <*> parseVar
--     <*> many space
--     <*> string "="
--     <*> many anyChar
--     <*> string ";"

-- parseVar :: Parsec String st String
-- parseVar = many anyChar
