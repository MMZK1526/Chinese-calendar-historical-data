module Model.Identifiers where

import           Data.KindID

data Identifiers = IDEra

instance ToPrefix 'IDEra where
  type PrefixSymbol _ = "era"
