module Main where

import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
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
