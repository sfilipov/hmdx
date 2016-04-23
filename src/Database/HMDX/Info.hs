{-# LANGUAGE TemplateHaskell #-}
module Database.HMDX.Info
    ( defaultDBSettings
    , createDB
    ) where

import           Database.HMDX.SSAS ( defaultSettings
                                    , catalogName
                                    , cubeName
                                    , mdSchemaDimensions
                                    , mdSchemaHierarchies
                                    , mdSchemaLevels)

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
  -- runIO $ putStrLn $ pprint qs
  hText <- runIO $ mdSchemaHierarchies transport settings
  let hierarchies = map (toTitleCase . textToString) $ fst hText
  let hierarchiesUnique = map textToString $ snd hText
  let h = zip hierarchies hierarchiesUnique
  qhs <- createHierarchyRecords h
  -- runIO $ putStrLn $ pprint qhs
  lText <- runIO $ mdSchemaLevels transport settings
  let levels = map (toTitleCase . textToString) $ fst lText
  let levelsUnique = map textToString $ snd lText
  let l = zip levels levelsUnique
  qls <- createLevelRecords l
  -- runIO $ putStrLn $ pprint qls
  ir <- createCubeRecord $ unpack $ cube dbsettings
  -- runIO $ putStrLn $ pprint ir
  return $ qs ++ ir
  return $ qhs ++ ir
  -- return $ qs ++ qhs ++ qls ++ ir

createDimensionRecords :: [(String, String)] -> Q [Dec]
createDimensionRecords xs = return $ concat $ map createDimensionRecord xs

createDimensionRecord :: (String, String) -> [Dec]
createDimensionRecord (dimName, dimUnique) = [SigD name (ConT ''String), FunD name clausexs]
  where
    name = mkName ("dim" ++ dimName)
    body = NormalB (LitE (StringL dimUnique))
    clausexs = [Clause [] body []]

createHierarchyRecords :: [(String, String)] -> Q [Dec]
createHierarchyRecords xs = return $ concat $ map createHierarchyRecord xs

createHierarchyRecord :: (String, String) -> [Dec]
createHierarchyRecord (hieName, hieUnique) = [SigD name (ConT ''String), FunD name clausexs]
  where
    name = mkName ("hie" ++ stripBad hieUnique)
    body = NormalB (LitE (StringL hieUnique))
    clausexs = [Clause [] body []]

createLevelRecords :: [(String, String)] -> Q [Dec]
createLevelRecords xs = return $ concat $ map createLevelRecord xs

createLevelRecord :: (String, String) -> [Dec]
createLevelRecord (hieName, hieUnique) = [SigD name (ConT ''String), FunD name clausexs]
  where
    name = mkName ("lvl" ++ stripBad hieUnique)
    body = NormalB (LitE (StringL hieUnique))
    clausexs = [Clause [] body []]


stripBad :: String -> String
stripBad [] = []
stripBad (x:xs)
    | x == '[' = stripBad xs
    | x == ']' = stripBad xs
    | x == '.' = stripBad xs
    | x == '-' = stripBad xs
    | x == '(' = stripBad xs
    | x == ')' = stripBad xs
    | x == ' ' = stripBad xs
    | otherwise = x : stripBad xs

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
