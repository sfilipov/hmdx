module Main where

import           BasicPrelude
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Database.OLAP ( defaultSettings
                               , catalogName
                               , cubeName
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
        }
    transport <- initTransport url addAuth id
    putStrLn "!!! List of all dimensions:"
    mdSchemaDimensions transport settings >>= print
    putStrLn "!!! Number of hierarchies:"
    hierarchies <- mdSchemaHierarchies transport settings
    print $ length hierarchies
    putStrLn "!!! Number of levels:"
    levels <- mdSchemaLevels transport settings
    print $ length levels

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"
