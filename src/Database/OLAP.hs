{-# LANGUAGE OverloadedStrings #-}

module Database.OLAP
    ( discoverProperty
    , executeMdx
    ) where

import           Data.Text (Text)
import           Network.SOAP (invokeWS, Transport, ResponseParser(CursorParser))
import           Network.SOAP.Parsing.Cursor (readT)
import           Text.XML.Cursor hiding (element, content)
import           Text.XML.Writer (elementA, element, content)

type PropertyName = Text

discoverProperty :: Transport -> PropertyName -> IO Text
discoverProperty t property = invokeWS t action header body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Discover"

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

type MdxQuery = Text

executeMdx :: Transport -> MdxQuery -> IO Text
executeMdx t query = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Execute"

    body = elementA "Execute" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "Command" $ element "Statement" $ content query
             element "Properties" $ element "PropertyList" $ do
               element "Catalog" $ content "AdventureWorksDW2012Multidimensional-SE"
               element "Dialect" $ content "MDX"

    parser :: Cursor -> Text
    parser cur = readT "Value" row
                 where row = head $ cur $// laxElement "row"
