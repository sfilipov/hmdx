module Main where

import           BasicPrelude
import           Network.HTTP.Client (Request, applyBasicAuth)
import           Network.SOAP.Transport.HTTP (initTransport)
import           Database.OLAP (executeMdx)

main :: IO ()
main = do
    let ip = "localhost:8080"
    let url = "http://" ++ ip ++ "/OLAP/msmdpump.dll"
    let query1 = "SELECT {[Measures].[Internet Freight Cost], [Measures].[Internet Tax Amount]} ON COLUMNS, {[Date].[Fiscal].[Fiscal Year]} ON ROWS FROM [Adventure Works] WHERE {[Sales Territory].[Sales Territory Country].&[United Kingdom]}"
    transport <- initTransport url addAuth id
    executeMdx transport query1 >>= print

addAuth :: Request -> Request
addAuth req = applyBasicAuth "WIN-SSAS\\ReadUser" "Password01" req
