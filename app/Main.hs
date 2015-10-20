{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP (invokeWS, Transport, ResponseParser(CursorParser))
import           Network.SOAP.Transport.HTTP (initTransport)
import           Network.SOAP.Parsing.Cursor (Dict, dictBy, readT, readC)
import           Text.XML.Cursor hiding (element, content)
import           Text.XML.Writer (elementA, element, content, comment)

main :: IO ()
main = do
    let ip = "172.28.128.5"
    let url = "http://" ++ ip ++ "/OLAP/msmdpump.dll"
    transport <- initTransport url addAuth id
    response <- getResponse transport
    print response

addAuth :: Request -> Request
addAuth req = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01" req

getResponse :: Transport -> IO Dict
getResponse t = invokeWS t discAction header body parser
  where
    discAction = "urn:schemas-microsoft-com:xml-analysis:Discover"
    execAction = "urn:schemas-microsoft-com:xml-analysis:Execute"

    header = elementA "Version"
        [ ("xmlns", "http://schemas.microsoft.com/analysisservices/2008/engine/100")
        , ("Sequence", "400")
        ] $ comment "asdasd"

    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "RequestType" $ content "DISCOVER_PROPERTIES"
             element "Restrictions" $ element "RestrictionList"
                                    $ element "PropertyName"
                                    $ content "DbpropMsmdSubqueries"
             element "Properties" $ element "PropertyList" $ comment "asdasd"

    parser = dictBy "return"
