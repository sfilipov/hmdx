module Main where

import           BasicPrelude
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Database.OLAP ( defaultSettings
                               , catalogName
                               , cubeName
                               , dimensionName
                               , hierarchyName
                               , mdSchemaDimensions
                               , mdSchemaHierarchies
                               , mdSchemaLevels)

main :: IO ()
main = do
    let ip = "localhost:8080"
    let url = "http://" ++ ip ++ "/OLAP/msmdpump.dll"
    let settings = defaultSettings {
      catalogName = "AdventureWorksDW2012Multidimensional-SE"
    , cubeName = "Adventure Works"
    , dimensionName = "[Geography]"
    , hierarchyName = "[Geography].[Geography]"
    }
    transport <- initTransport url addAuth id
    putStrLn "!!! List of all dimensions:"
    mdSchemaDimensions transport settings >>= print
    putStrLn $ "!!! List all hierarchies of " ++ dimensionName settings
    mdSchemaHierarchies transport settings >>= print
    putStrLn $ "!!! List all levels of " ++ hierarchyName settings
    mdSchemaLevels transport settings >>= print

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"
