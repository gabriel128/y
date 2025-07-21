{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Types.Parsing where

import Data.Text
import qualified Data.Text as T
import Parser.Defs
import Text.Megaparsec
import Text.Megaparsec.Char
import Types.Defs (Type (..))

fromTypeId :: T.Text -> Either T.Text (Maybe Type)
fromTypeId tid =
    case T.toLower tid of
        "i64" -> Right . Just $ I64
        "u64" -> Right . Just $ U64
        "bool" -> Right . Just $ TyBool
        "()" -> Right . Just $ Unit
        someId -> Left $ T.pack "Not valid type " <> someId

parseTypeId :: Parser (Maybe Type)
parseTypeId =
    label "type identifier" . failsIfError . lexeme $ fmap (Types.Parsing.fromTypeId . pack) $ (:) <$> letterChar <*> many alphaNumChar
  where
    failsIfError :: Parser (Either Text (Maybe Type)) -> Parser (Maybe Type)
    failsIfError parserEither = do
        res <- parserEither
        case res of
            (Right ty) -> return ty
            (Left err) -> fail (unpack err)

-- parseTypeInfo :: Parser TypeMeta
-- parseTypeInfo = label "mut" . lexeme $
--     do
--         void space
--         typeInfo <- option "" (symbol "mut")
--         return (if typeInfo == "mut" then TypeMeta True else TypeMeta False)
