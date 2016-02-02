module Main where

import           BasicPrelude
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Database.OLAP (mdSchemaDimensions)

main :: IO ()
main = do
    let ip = "localhost:8080"
    let url = "http://" ++ ip ++ "/OLAP/msmdpump.dll"
    let catalogName = "AdventureWorksDW2012Multidimensional-SE"
    let cubeName = "Adventure Works"
    transport <- initTransport url addAuth id
    dimensions <- mdSchemaDimensions transport catalogName cubeName
    print dimensions

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"
