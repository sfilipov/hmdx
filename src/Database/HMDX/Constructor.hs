module Database.HMDX.Constructor where

import           Data.List (intercalate)
import           Prelude


data SetExpr = SetExpr [String] deriving (Eq)
instance Show SetExpr where
  show (SetExpr xs) = "{ " ++ separateWithComma xs ++ " }"
    where
      separateWithComma = intercalate ", "


data QueryAxisExpr = QAE SetExpr Int deriving (Eq)
instance Show QueryAxisExpr where
  show (QAE se i) = show se ++ " ON " ++ show i


data SubcubeExpr = CubeName String
                 | Subcube MdxExpr
                 deriving (Eq)
instance Show SubcubeExpr where
  show (CubeName s) = s
  show (Subcube mdx) = show mdx


data MdxExpr = MdxExpr { axes    :: [QueryAxisExpr]
                       , subcube :: SubcubeExpr
                       , slicer  :: Maybe SetExpr
                       } deriving (Eq)
instance Show MdxExpr where
  show mdx@MdxExpr {slicer = maybeWhere} = showSelect ++ showFrom ++ showWhere maybeWhere
    where
      showSelect = "SELECT " ++ showAnyList (axes mdx)  ++ "\n"
      showFrom = "FROM " ++ show (subcube mdx) ++ "\n"
      showWhere Nothing = ""
      showWhere (Just setExpr) = "WHERE " ++ show setExpr


showAnyList ::(Show a) => [a] -> String
showAnyList [] = ""
showAnyList [x] = show x
showAnyList (x:xs) = show x ++ ", " ++ showAnyList xs
