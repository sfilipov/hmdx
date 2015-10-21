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
import           Database.OLAP (discoverProperty)
main :: IO ()
main = do
    let ip = "172.28.128.5"
    let url = "http://" ++ ip ++ "/OLAP/msmdpump.dll"
    transport <- initTransport url addAuth id
    response <- discoverProperty transport "Catalog"
    print response

addAuth :: Request -> Request
addAuth req = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01" req
