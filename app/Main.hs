{-# LANGUAGE TemplateHaskell #-}
module Main where

import           BasicPrelude
import           Database.HMDX.Info
import           Database.HMDX.Deconstructor
import           Language.Haskell.TH

$(createDB defaultDBSettings)

main :: IO ()
main = do
    let result = expQToMDX express
    print result

express :: ExpQ
express = [| do
  (_, d, m) <- cube
  -- guard $ isDescendantOf (Date "2016")
  -- guard (p == Product "Bike")
  return (m, d) |]
