module Model.Identifiers where

import           Data.KindID.Class

data Identifiers = IDEra

instance ToPrefix 'IDEra where
  type PrefixSymbol _ = "era"
