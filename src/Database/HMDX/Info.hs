{-# LANGUAGE TemplateHaskell #-}
module Database.HMDX.Info
    ( createDB
    ) where

import           Database.HMDX.SSAS ( defaultSettings
                                    , catalogName
                                    , cubeName
                                    , mdSchemaDimensions)

import           BasicPrelude hiding (putStrLn, words)
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Language.Haskell.TH
import           Data.Char (toUpper)
import           Prelude (putStrLn, words)


{-#NOINLINE createDB #-}
createDB :: Q [Dec]
createDB = do
  let ip = "localhost:8080"
  let url = "http://" ++ ip ++ "/OLAP/msmdpump.dll"
  let settings = defaultSettings {
        catalogName = "AdventureWorksDW2012Multidimensional-SE"
      , cubeName = "Adventure Works"
      }
  transport <- runIO $ initTransport url addAuth id
  dText <- runIO $ mdSchemaDimensions transport settings
  let dimensions = map (toTitleCase . textToString) dText
  qs <- createDimensionRecords dimensions
  runIO $ putStrLn $ pprint qs
  cr <- createIntersectionRecord dimensions
  runIO $ putStrLn $ pprint cr
  return qs

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"

createDimensionRecords :: [String] -> Q [Dec]
createDimensionRecords xs = return $ map createDimensionRecord xs

createDimensionRecord :: String -> Dec
createDimensionRecord dimName = DataD context name vars cons derives
  where
    context = []
    name = mkName dimName
    vars = []
    cons = [NormalC name [field]]
    field = (NotStrict, ConT ''String)
    derives = [''Show]

createIntersectionRecord :: [String] -> Q Dec
createIntersectionRecord dimensions = return $ DataD context name vars cons derives
  where
    context = []
    name = mkName "Intersection"
    vars = []
    field = (NotStrict, createDimensionsTuple (reverse dimensions) $ length dimensions)
    cons = [NormalC name [field]]
    derives = [''Show]


type NumberOfDimensions = Int
createDimensionsTuple :: [String] -> NumberOfDimensions -> Type
createDimensionsTuple []     _ = error "List of dimensions should not be empty!"
createDimensionsTuple [x]    n = AppT (TupleT n) (ConT $ mkName x)
createDimensionsTuple (x:xs) n = AppT (createDimensionsTuple xs n) (ConT $ mkName x)


toTitleCase :: String -> String
toTitleCase =  concat . map capitaliseWord . words

capitaliseWord :: String -> String
capitaliseWord [] = ""
capitaliseWord (x:xs) = toUpper x : xs
