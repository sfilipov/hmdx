module Database.HMDX.Constructor where

import           Data.List (intercalate)
import           Prelude


data SetExpr = SetExpr [String] deriving (Eq)
instance Show SetExpr where
  show (SetExpr xs) = "{ " ++ separateWithComma xs ++ " }"
    where
      separateWithComma = intercalate ", "


data QueryExpr = Star
               | QueryExpr [(SetExpr, Int)]
               deriving (Eq)
instance Show QueryExpr where
  show Star = "*"
  show (QueryExpr exprs) = showAxes exprs
    where
      showAxes [] = ""
      showAxes [x] = showTuple x
      showAxes (x:xs) = showTuple x ++ ", " ++ showAxes xs
      showTuple (se, int) = show se ++ " ON " ++ show int


data SubcubeExpr = CubeName String
                 | Subcube MdxExpr
                 deriving (Eq)
instance Show SubcubeExpr where
  show (CubeName s) = s
  show (Subcube mdx) = show mdx


data MdxExpr = MdxExpr { axes    :: QueryExpr
                       , subcube :: SubcubeExpr
                       , slicer  :: Maybe SetExpr
                       } deriving (Eq)
instance Show MdxExpr where
  show mdx@MdxExpr {slicer = maybeWhere} = showSelect ++ showFrom ++ showWhere maybeWhere
    where
      showSelect = "SELECT " ++ show (axes mdx)  ++ "\n"
      showFrom = "FROM " ++ show (subcube mdx) ++ "\n"
      showWhere Nothing = ""
      showWhere (Just setExpr) = "WHERE " ++ show setExpr
