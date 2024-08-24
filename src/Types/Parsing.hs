{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Types.Parsing where
import qualified Data.Text as T
import Types.Defs (Type(..), NativeType(TyInt))

fromTypeId :: T.Text -> Either T.Text Type
fromTypeId tid =
  case T.toLower tid of
    "int" -> Right (Native TyInt)
    someId ->  Left $ T.pack "Not valid type " <> someId
