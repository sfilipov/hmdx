{-# LANGUAGE TemplateHaskell #-}
module Database.HMDX.Info
    ( createDB
    ) where

import           Database.HMDX.SSAS ( defaultSettings
                                    , catalogName
                                    , cubeName
                                    , mdSchemaDimensions)

import           BasicPrelude hiding (putStrLn)
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Language.Haskell.TH
import           Data.Char (toUpper, isSpace)
import           Prelude (putStrLn)


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
  dimensions <- runIO $ mdSchemaDimensions transport settings
  qs <- createDimensionRecords dimensions
  runIO $ putStrLn $ pprint qs
  return qs

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"

createDimensionRecords :: [Text] -> Q [Dec]
createDimensionRecords xs = return $ map createDimensionRecord xs

createDimensionRecord :: Text -> Dec
createDimensionRecord dimName = DataD context name vars cons derives where
  context = []
  name = mkName $ toTitleCase $ removeSpace $ textToString dimName
  vars = []
  cons = [NormalC name [field]]
  field = (NotStrict, ConT ''String)
  derives = [''Show]

toTitleCase :: String -> String
toTitleCase [] = ""
toTitleCase [x] = [toUpper x]
toTitleCase (x:xs) = toUpper x : xs

removeSpace :: String -> String
removeSpace [] = ""
removeSpace (x:xs)
    | isSpace x = removeSpace xs
    | otherwise = x : removeSpace xs
