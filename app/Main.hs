{-# LANGUAGE TemplateHaskell #-}
module Main where

import           BasicPrelude
import           Database.HMDX.Info
import           Database.HMDX.Constructor


createDB

main :: IO ()
main = do
    let selects = [ QAE "[Product].[Category]" 0
                  , QAE "[Measure]" 1]
    let froms = CubeName "Adventure Works"
    let mdx = MdxExpr { select = selects, from = froms}
    print mdx
