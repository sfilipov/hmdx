module Database.HMDX.Constructor where

import           Prelude

data QueryAxisExpr = QAE String Int deriving (Eq)
instance Show QueryAxisExpr where
  show (QAE s i) = s ++ " ON " ++ show i

data SubcubeExpr = CubeName String deriving (Eq)
instance Show SubcubeExpr where
  show (CubeName s) = s

data MdxExpr = MdxExpr { select :: [QueryAxisExpr]
                       , from   :: SubcubeExpr
                       } deriving (Eq)

instance Show MdxExpr where
  show q = showSelect ++ showFrom
    where
      showSelect = "SELECT " ++ showAnyList (select q)  ++ "\n"
      showFrom = "FROM " ++ show (from q)

showAnyList ::(Show a) => [a] -> String
showAnyList [] = ""
showAnyList [x] = show x
showAnyList (x:xs) = show x ++ ", " ++ showAnyList xs
