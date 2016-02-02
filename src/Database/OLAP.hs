module Database.OLAP
    ( mdSchemaDimensions
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

type CatalogName = Text
type CubeName = Text
type DimensionName = Text
type HierarchyName = Text
type MdxQuery = Text

mdSchemaDimensions :: Transport -> CatalogName -> CubeName -> IO [Text]
mdSchemaDimensions t catalog cube = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Discover"

    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "RequestType" $ content "MDSCHEMA_DIMENSIONS"
             element "Restrictions" $ element "RestrictionList"
                                    $ element "CUBE_NAME"
                                    $ content cube
             element "Properties" $ element "PropertyList"
                                  $ element "Catalog"
                                  $ content catalog

    parser :: Cursor -> [Text]
    parser c = dimensions
      where
        rows = c $// laxElement "row"
        dimensions = fmap (readT "DIMENSION_UNIQUE_NAME") rows


mdSchemaHierarchies :: Transport -> CatalogName -> CubeName -> DimensionName -> IO [Text]
mdSchemaHierarchies t catalog cube dimension = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Discover"

    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
      element "RequestType" $ content "MDSCHEMA_HIERARCHIES"
      element "Restrictions" $ element "RestrictionList" $ do
        element "CUBE_NAME" $ content cube
        element "DIMENSION_UNIQUE_NAME" $ content dimension
      element "Properties" $ element "PropertyList"
                          $ element "Catalog"
                          $ content catalog

    parser :: Cursor -> [Text]
    parser c = dimensions
      where
        rows = c $// laxElement "row"
        dimensions = fmap (readT "HIERARCHY_UNIQUE_NAME") rows


mdSchemaLevels :: Transport -> CatalogName -> CubeName -> HierarchyName -> IO [Text]
mdSchemaLevels t catalog cube hierarchy = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Discover"

    body = elementA "Discover" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
      element "RequestType" $ content "MDSCHEMA_LEVELS"
      element "Restrictions" $ element "RestrictionList" $ do
        element "CUBE_NAME" $ content cube
        element "HIERARCHY_UNIQUE_NAME" $ content hierarchy
      element "Properties" $ element "PropertyList"
                          $ element "Catalog"
                          $ content catalog

    parser :: Cursor -> [Text]
    parser c = dimensions
      where
        rows = c $// laxElement "row"
        dimensions = fmap (readT "LEVEL_UNIQUE_NAME") rows


executeMdx :: Transport -> CatalogName -> MdxQuery -> IO (IntMap [Text], IntMap Double)
executeMdx t catalog query = invokeWS t action () body (CursorParser parser)
  where
    action = "urn:schemas-microsoft-com:xml-analysis:Execute"

    body = elementA "Execute" [("xmlns","urn:schemas-microsoft-com:xml-analysis")] $ do
             element "Command" $ element "Statement" $ content query
             element "Properties" $ element "PropertyList" $ do
               element "Catalog" $ content catalog
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
