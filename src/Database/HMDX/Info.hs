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
  ir <- createCubeRecord dimensions
  runIO $ putStrLn $ pprint ir
  return $ qs ++ [ir]

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"

createDimensionRecords :: [String] -> Q [Dec]
createDimensionRecords xs = return $ map createDimensionRecord xs

createDimensionRecord :: String -> Dec
createDimensionRecord dimName = FunD name clausexs
  where
    name = mkName ("dim" ++ dimName)
    body = NormalB (LitE (StringL dimName))
    clausexs = [Clause [] body []]

createCubeRecord :: [String] -> Q Dec
createCubeRecord dimensions = return $ FunD name clausexs
  where
    name = mkName "cube"
    dimToFunName dimName = mkName ("dim" ++ dimName)
    funVarEs = fmap (VarE . dimToFunName) dimensions
    body = NormalB (TupE funVarEs)
    clausexs = [Clause [] body []]

toTitleCase :: String -> String
toTitleCase =  concat . map capitaliseWord . words

capitaliseWord :: String -> String
capitaliseWord [] = ""
capitaliseWord (x:xs) = toUpper x : xs
