module Main where

import           BasicPrelude
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Database.OLAP (mdSchemaDimensions, mdSchemaHierarchies)

main :: IO ()
main = do
    let ip = "localhost:8080"
    let url = "http://" ++ ip ++ "/OLAP/msmdpump.dll"
    let catalogName = "AdventureWorksDW2012Multidimensional-SE"
    let cubeName = "Adventure Works"
    let dimensionName = "[Product]"
    transport <- initTransport url addAuth id
    putStrLn "!!! List of all dimensions:"
    mdSchemaDimensions transport catalogName cubeName >>= print
    putStrLn $ "!!! List all hierarchies of " ++ show dimensionName
    mdSchemaHierarchies transport catalogName cubeName dimensionName >>= print

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"
