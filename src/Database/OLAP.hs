module Database.OLAP
    ( discoverProperty
    , executeMdx
    ) where

import           ClassyPrelude
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
                 where row = unsafeHead $ cur $// laxElement "row"

type MdxQuery = Text

executeMdx :: Transport -> MdxQuery -> IO (IntMap [Text])
executeMdx t query = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Execute"

    body = elementA "Execute" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "Command" $ element "Statement" $ content query
             element "Properties" $ element "PropertyList" $ do
               element "Catalog" $ content "AdventureWorksDW2012Multidimensional-SE"
               element "Dialect" $ content "MDX"

    parser :: Cursor -> IntMap [Text]
    parser cur = allM
      where
        members0 = cur $// attributeIs "name" "Axis0" &// laxElement "Member"
        members1 = cur $// attributeIs "name" "Axis1" &// laxElement "Member"
        toCapt   = fmap (readT "Caption")
        allM     = asIntMap $ insertMap 1 (toCapt members1) $ singletonMap 0 (toCapt members0)
