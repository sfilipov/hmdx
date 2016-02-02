module Database.OLAP
    ( defaultSettings
    , catalogName
    , cubeName
    , dimensionName
    , hierarchyName
    , mdSchemaDimensions
    , mdSchemaHierarchies
    , mdSchemaLevels
    , executeMdx
    ) where

import           BasicPrelude
import           Data.IntMap as M
import           Network.SOAP (invokeWS, Transport, ResponseParser(CursorParser))
import           Network.SOAP.Parsing.Cursor (readT)
import           Text.XML.Cursor hiding (element, content)
import           Text.XML.Writer (elementA, element, content)

type MdxQuery = Text

data SearchSettings = SearchSettings
    { catalogName :: Text
    , cubeName :: Text
    , dimensionName :: Text
    , hierarchyName :: Text
    }

defaultSettings :: SearchSettings
defaultSettings = SearchSettings "" "" "" ""

mdSchemaDimensions :: Transport -> SearchSettings -> IO [Text]
mdSchemaDimensions t settings = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Discover"

    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "RequestType" $ content "MDSCHEMA_DIMENSIONS"
             element "Restrictions" $ element "RestrictionList"
                                    $ element "CUBE_NAME"
                                    $ content $ cubeName settings
             element "Properties" $ element "PropertyList"
                                  $ element "Catalog"
                                  $ content $ catalogName settings

    parser :: Cursor -> [Text]
    parser c = dimensions
      where
        rows = c $// laxElement "row"
        dimensions = fmap (readT "DIMENSION_UNIQUE_NAME") rows


mdSchemaHierarchies :: Transport -> SearchSettings -> IO [Text]
mdSchemaHierarchies t settings = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Discover"

    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
      element "RequestType" $ content "MDSCHEMA_HIERARCHIES"
      element "Restrictions" $ element "RestrictionList" $ do
        element "CUBE_NAME" $ content $ cubeName settings
        element "DIMENSION_UNIQUE_NAME" $ content $ dimensionName settings
      element "Properties" $ element "PropertyList"
                          $ element "Catalog"
                          $ content $ catalogName settings

    parser :: Cursor -> [Text]
    parser c = dimensions
      where
        rows = c $// laxElement "row"
        dimensions = fmap (readT "HIERARCHY_UNIQUE_NAME") rows


mdSchemaLevels :: Transport -> SearchSettings -> IO [Text]
mdSchemaLevels t settings = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Discover"

    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
      element "RequestType" $ content "MDSCHEMA_LEVELS"
      element "Restrictions" $ element "RestrictionList" $ do
        element "CUBE_NAME" $ content $ cubeName settings
        element "HIERARCHY_UNIQUE_NAME" $ content $ hierarchyName settings
      element "Properties" $ element "PropertyList"
                          $ element "Catalog"
                          $ content $ catalogName settings

    parser :: Cursor -> [Text]
    parser c = dimensions
      where
        rows = c $// laxElement "row"
        dimensions = fmap (readT "LEVEL_UNIQUE_NAME") rows


executeMdx :: Transport -> SearchSettings -> MdxQuery -> IO (IntMap [Text], IntMap Double)
executeMdx t settings query = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Execute"

    body = elementA "Execute" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "Command" $ element "Statement" $ content query
             element "Properties" $ element "PropertyList" $ do
               element "Catalog" $ content $ catalogName settings
               element "Dialect" $ content "MDX"

    toMembers :: Cursor -> IntMap [Text]
    toMembers cur = allM
      where
        members0 = cur $// attributeIs "name" "Axis0" &// laxElement "Member"
        members1 = cur $// attributeIs "name" "Axis1" &// laxElement "Member"
        toCapt   = fmap (readT "Caption")
        allM     = M.insert 1 (toCapt members1) $ M.singleton 0 (toCapt members0)

    toCells :: Cursor -> IntMap Double
    toCells cur = allC
      where
        cells = cur $// laxElement "Cell"
        readA a c = read $ concat $ c $| attribute a
        ordinals = fmap (readA "CellOrdinal") cells
        values = fmap (readT "Value") cells
        dValues = fmap (read :: Text -> Double) values
        allC  = M.fromList $ zip ordinals dValues

    parser :: Cursor -> (IntMap [Text], IntMap Double)
    parser c = (toMembers c, toCells c)
