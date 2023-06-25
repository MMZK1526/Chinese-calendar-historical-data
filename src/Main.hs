module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype Literal = Literal { unLiteral :: Text }
  deriving newtype (Eq, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance Show Literal where
  show :: Literal -> String
  show = (++ "\"") . ('"' :) . concatMap showChar . T.unpack . unLiteral
    where
      showChar ch
        | not (isAscii ch) && isPrint ch = [ch]
        | otherwise                      = init . tail $ show ch

data Emperor = Emperor { name :: Literal, eras :: [Era] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Era = Era { name :: Literal }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

foo :: IO (Maybe [Dynasties])
foo = decode <$> BSL.readFile "data/dynasties.json"
