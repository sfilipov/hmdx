{-# LANGUAGE OverloadedStrings #-}

module Database.OLAP
    ( discoverProperty
    ) where

import           Data.Text (Text)
import           Network.SOAP (invokeWS, Transport, ResponseParser(CursorParser))
import           Network.SOAP.Parsing.Cursor (readT)
import           Text.XML.Cursor hiding (element, content)
import           Text.XML.Writer (elementA, element, content)

type PropertyName = Text

discoverProperty :: Transport -> PropertyName -> IO Text
discoverProperty t property = invokeWS t discAction header body (CursorParser parser)
  where
    discAction = "urn:schemas-microsoft-com:xml-analysis:Discover"

    header = elementA "Version"
        [ ("xmlns", "http://schemas.microsoft.com/analysisservices/2008/engine/100")
        , ("Sequence", "400")
        ] $ ()
    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "RequestType" $ content "DISCOVER_PROPERTIES"
             element "Restrictions" $ element "RestrictionList"
                                    $ element "PropertyName"
                                    $ content property
             element "Properties" $ element "PropertyList" $ ()

    parser :: Cursor -> Text
    parser cur = readT "Value" row
                 where row = head $ cur $// laxElement "row"
