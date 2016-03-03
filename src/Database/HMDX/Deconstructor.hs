{-# LANGUAGE TemplateHaskell #-}
module Database.HMDX.Deconstructor where

import BasicPrelude
import Control.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.List(partition)
import Database.HMDX.Constructor
import System.IO.Unsafe

expQToMDX :: ExpQ -> MdxExpr
expQToMDX = unsafePerformIO . runQ . fmap expToMDX

expToMDX :: Exp -> MdxExpr
expToMDX (DoE stmts) = MdxExpr {
    axes = returnStatements stmts,
    subcube = bindStatements stmts,
    slicer = guardStatements stmts
}
expToMDX _ = error "Currently only do statements are supported"

returnStatements :: [Stmt] -> QueryExpr
returnStatements _ = QueryExpr [ (SetExpr ["[Product].[Category]"], 0)
                               , (SetExpr ["[Measure]"], 1)
                               ]

bindStatements :: [Stmt] -> SubcubeExpr
bindStatements _ = CubeName "[Adventure Works]"

guardStatements :: [Stmt] -> Maybe SetExpr
guardStatements _ = Just (SetExpr ["[Geography].[Country].&[United Kingdom]"])
