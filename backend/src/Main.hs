module Main where

import Data.KindID
import Model.Era

main :: IO ()
main = do
    tids <- genEraIDs 2
    print tids
