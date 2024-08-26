{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Types.Parsing where
import qualified Data.Text as T
import Types.Defs (Type(..), NativeType(..))

fromTypeId :: T.Text -> Either T.Text Type
fromTypeId tid =
  case T.toLower tid of
    "i64" -> Right (Native I64)
    someId ->  Left $ T.pack "Not valid type " <> someId
