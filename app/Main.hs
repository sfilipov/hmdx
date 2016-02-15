{-# LANGUAGE TemplateHaskell #-}
module Main where

import           BasicPrelude
import           Database.HMDX.Info
import           Database.HMDX.Constructor


createDB

main :: IO ()
main = do
    let a = [ QAE (SetExpr ["[Product].[Category]"]) 0
            , QAE (SetExpr ["[Measure]"]) 1
            ]
    let subcubes = CubeName "[Adventure Works]"
    let wh = Just (SetExpr ["[Geography].[Country].&[United Kingdom]"])
    let mdx = MdxExpr { axes = a, subcube = subcubes, slicer = wh}
    print mdx
