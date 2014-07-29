-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module XmlParser(parseFile) where

import Prelude hiding(FilePath, readFile)
import Data.Maybe(fromMaybe)
import Text.XML
import Text.Printf(printf)
import Filesystem.Path.CurrentOS(FilePath)
import Data.Text(unpack, Text, strip)
import Control.Applicative

import qualified Data.Map as M

import Gpx

-----------------------------------------------------------------------------------------------------------------------

parseFile :: FilePath -> IO (Gpx, Document)
parseFile path = do
   doc@(Document _ root _) <- readFile def path
   return (parseGpx root, doc)


(<=>) :: Name -> Name -> Bool
n0 <=> n1 = nameLocalName n0 == nameLocalName n1

infix 4 <=>


textToDouble :: Text -> Double
textToDouble = read . unpack


parsePoint :: Element -> Point
parsePoint element = 
   Point 
   {
      lat = textToDouble $ lookupAttr "lat" element,
      lon = textToDouble $ lookupAttr "lon" element
   }

parseRoutePoint :: Element -> RoutePoint
parseRoutePoint routePointElem =
   RoutePoint
   {
      rteptPoint = parsePoint routePointElem,
      rteptExtensions = parseExtensions <$> lookupChildElement "extensions" routePointElem
   } 

parseExtensions :: Element -> Extensions
parseExtensions extensionsElement =
   Extensions
   { 
      extRoutePointExtension = parseRoutePointExtension $ 
         lookupChildElement1 "RoutePointExtension" extensionsElement
   }  
   
parseRoutePointExtension :: Element -> RoutePointExtension
parseRoutePointExtension routePointExtensionElement = 
   RoutePointExtension
   {
      rpePoints = map parsePoint $ lookupChildElements "rpt" routePointExtensionElement
   }

parseRoute :: Element -> Route
parseRoute routeElem = 
   Route 
   { 
      rteName = lookupChildContent "name" routeElem, 
      rtePoints = map parseRoutePoint $ lookupChildElements "rtept" routeElem
   }


parseGpx :: Element -> Gpx
parseGpx gpxElem = 
   Gpx
   { 
      gpxCreator = lookupAttr "creator" gpxElem,
      gpxVersion = lookupAttr "version" gpxElem,
      gpxSchemaLocation = lookupAttr "schemaLocation" gpxElem,
      gpxRoutes = map parseRoute $ lookupChildElements "rte" gpxElem
   }


lookupAttr :: Name -> Element -> Text
lookupAttr name element = 
   let 
      msg = printf "Attribute '%s' not found." $ show name
   in
      fromMaybe (error msg) $ M.lookup name $ elementAttributes element
      
      
lookupChildElements :: Name -> Element -> [Element]
lookupChildElements name parentElem =
   map nodeToElement $ filter filterFun $ elementNodes parentElem 
   where
      nodeToElement (NodeElement childElem) = childElem
      nodeToElement _ = undefined
   
      filterFun (NodeElement childElem) = elementName childElem <=> name
      filterFun _ = False

   
lookupChildElement1 :: Name -> Element -> Element
lookupChildElement1  name parentElem = 
   fromMaybe (error $ printf "Element %s not found." (show name)) $ lookupChildElement name parentElem


lookupChildElement :: Name -> Element -> Maybe Element
lookupChildElement name parentElem = case lookupChildElements name parentElem of
   []      -> Nothing
   [child] -> Just child
   _       -> error $ printf "More than one Element %s found." $ show name


lookupChildContent :: Name -> Element -> Maybe Text
lookupChildContent name parentElem = do
   child <- lookupChildElement name parentElem
   lookupContent child
   where
      lookupContent (Element _ _ [NodeContent content]) = Just $ strip content
      lookupContent  _ = Nothing

      

-----------------------------------------------------------------------------------------------------------------------
