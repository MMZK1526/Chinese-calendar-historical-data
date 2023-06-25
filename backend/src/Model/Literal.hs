module Model.Literal where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T

-- | A "Text" wrapper that prints Unicode characters as-is.
newtype Literal = Literal { unLiteral :: Text }
  deriving newtype (Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance Show Literal where
  show :: Literal -> String
  show = (++ "\"") . ('"' :) . concatMap showChar . T.unpack . unLiteral
    where
      showChar ch
        | not (isAscii ch) && isPrint ch = [ch]
        | otherwise                      = init . tail $ show ch
