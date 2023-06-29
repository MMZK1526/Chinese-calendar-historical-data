module Model.Dynasty where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           GHC.Generics
import           Model.Literal

data Dynasty = Dynasty { name :: Literal, emperors :: [Emperor] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Emperor = Emperor { name :: Literal, eras :: [Era] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Era = Era { name :: Literal, index :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

loadDynasty :: IO [Dynasty]
loadDynasty = BSL.readFile "../data/dynasties.json" >>= throwDecode
