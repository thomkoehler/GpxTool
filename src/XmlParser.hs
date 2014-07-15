-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module XmlParser where

import Prelude hiding(FilePath, readFile)
import Data.Maybe(fromMaybe)
import Text.XML
import Text.Printf(printf)
import Filesystem.Path.CurrentOS(FilePath)
import Data.Text(unpack, Text)

import qualified Data.Map as M

import Gpx

-----------------------------------------------------------------------------------------------------------------------

parseFile :: FilePath -> IO Gpx
parseFile path = do
   Document _ root _ <- readFile def path
   return $ parseGpx root


gpxName :: Text -> Name
gpxName localName = Name localName (Just "http://www.topografix.com/GPX/1/1") Nothing

gpxxName :: Text -> Name
gpxxName localName = Name localName (Just "http://www.garmin.com/xmlschemas/GpxExtensions/v3") Nothing
   

textToDouble :: Text -> Double
textToDouble = read . unpack


parseRoutePoint :: Element -> RoutePoint
parseRoutePoint routePointElem =
   RoutePoint
   {
      rteptLat = textToDouble $ lookupAttr "lat" routePointElem,
      rteptLon = textToDouble $ lookupAttr "lon" routePointElem,
      rteptExtensions = fmap parseExtensions $ lookupChildElement (gpxName "extensions") routePointElem
   } 

parseExtensions :: Element -> Extensions
parseExtensions extensionsElement =
   Extensions
   { 
      extRoutePointExtension = parseRoutePointExtension $ 
         lookupChildElement1 (gpxxName "RoutePointExtension") extensionsElement
   }  
   
parseRoutePointExtension :: Element -> RoutePointExtension
parseRoutePointExtension routePointExtensionElement = 
   RoutePointExtension
   {
      rpePoints = map parseExtensionRoutePoint $ lookupChildElements (gpxxName "rpt") routePointExtensionElement
   }


parseExtensionRoutePoint :: Element -> ExtensionRoutePoint
parseExtensionRoutePoint extensionRoutePointElement =
   ExtensionRoutePoint
   {
      erpLat = textToDouble $ lookupAttr "lat" extensionRoutePointElement,
      erpLon = textToDouble $ lookupAttr "lon" extensionRoutePointElement
   }   


parseRoute :: Element -> Route
parseRoute routeElem = 
   Route 
   { 
      rteName = lookupChildContent (gpxName "name") routeElem, 
      rtePoints = map parseRoutePoint $ lookupChildElements (gpxName "rtept") routeElem
   }


parseGpx :: Element -> Gpx
parseGpx gpxElem = 
   Gpx
   { 
      gpxRoute = map parseRoute $ lookupChildElements (gpxName "rte") gpxElem
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
   
      filterFun (NodeElement childElem) = elementName childElem == name
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
      lookupContent (Element _ _ [NodeContent content]) = Just content
      lookupContent  _ = Nothing

      

-----------------------------------------------------------------------------------------------------------------------