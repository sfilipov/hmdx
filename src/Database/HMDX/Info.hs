{-# LANGUAGE TemplateHaskell #-}
module Database.HMDX.Info
    ( defaultDBSettings
    , createDB
    ) where

import           Database.HMDX.SSAS ( defaultSettings
                                    , catalogName
                                    , cubeName
                                    , mdSchemaDimensions)

import           BasicPrelude hiding (putStrLn, words)
import           Network.HTTP.Client (applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Language.Haskell.TH
import           Data.Char (toUpper)
import           Data.Text (unpack)
import           Prelude (putStrLn, words)


data DBSettings = DBSettings
    { host :: Text
    , port :: Int
    , user :: Text
    , password :: Text
    , catalog :: Text
    , cube :: Text
    }

fullUrl :: DBSettings -> Text
fullUrl settings = "http://" ++ hostAndPort ++ "/OLAP/msmdpump.dll"
  where
    hostAndPort = host settings ++ ":" ++ show (port settings)

defaultDBSettings :: DBSettings
defaultDBSettings = DBSettings "localhost" 8080 "WIN-SSAS\\ReadUser" "Password01" "AdventureWorksDW2012Multidimensional-SE" "Adventure Works"

{-#NOINLINE createDB #-}
createDB :: DBSettings -> Q [Dec]
createDB dbsettings = do
  let url = unpack $ fullUrl dbsettings
  let u = encodeUtf8 $ user dbsettings
  let p = encodeUtf8 $ password dbsettings
  let settings = defaultSettings {
        catalogName = catalog dbsettings
      , cubeName = cube dbsettings
      }
  transport <- runIO $ initTransport url (applyBasicAuth u p) id
  dText <- runIO $ mdSchemaDimensions transport settings
  let dimensions = map (toTitleCase . textToString) $ fst dText
  let dimensionsUnique = map textToString $ snd dText
  let d = zip dimensions dimensionsUnique
  qs <- createDimensionRecords d
  runIO $ putStrLn $ pprint qs
  ir <- createCubeRecord $ unpack $ cube dbsettings
  runIO $ putStrLn $ pprint ir
  return $ qs ++ ir

createDimensionRecords :: [(String, String)] -> Q [Dec]
createDimensionRecords xs = return $ concat $ map createDimensionRecord xs

createDimensionRecord :: (String, String) -> [Dec]
createDimensionRecord (dimName, dimUnique) = [SigD name (ConT ''String), FunD name clausexs]
  where
    name = mkName ("dim" ++ dimName)
    body = NormalB (LitE (StringL dimUnique))
    clausexs = [Clause [] body []]

createCubeRecord :: String -> Q [Dec]
createCubeRecord cubeString = return [SigD name (ConT ''String), FunD name clausexs]
  where
    name = mkName "cube"
    cubeUniqueName = "[" ++ cubeString ++ "]"
    body = NormalB (LitE (StringL cubeUniqueName))
    clausexs = [Clause [] body []]

toTitleCase :: String -> String
toTitleCase =  concat . map capitaliseWord . words

capitaliseWord :: String -> String
capitaliseWord [] = ""
capitaliseWord (x:xs) = toUpper x : xs
