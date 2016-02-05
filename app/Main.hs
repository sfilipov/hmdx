{-# LANGUAGE TemplateHaskell #-}
module Main where

import           BasicPrelude
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Database.HMDX.SSAS ( defaultSettings
                                    , catalogName
                                    , cubeName
                                    , mdSchemaDimensions)
import           Database.HMDX.Info


createDB

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

addAuth :: Request -> Request
addAuth = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01"
