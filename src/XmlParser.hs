-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module XmlParser where

import Prelude hiding(FilePath, readFile)
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import Text.XML
import Text.Printf(printf)
import Filesystem.Path.CurrentOS(FilePath)
import Data.List(foldl')

import qualified Data.Map as M

import Gpx

-----------------------------------------------------------------------------------------------------------------------

parseFile :: FilePath -> IO Gpx
parseFile path = do
   Document _ root _ <- readFile def path
   return $ parseGpx root


gpxName :: Text -> Name
gpxName localName = Name localName (Just "http://www.topografix.com/GPX/1/1") Nothing
   

parseRoute :: Element -> Route
parseRoute routeElem = 
   Route 
   { 
      rteName = lookupChildContent (gpxName "name") routeElem, 
      rtePoints = [] 
   }

parseGpx :: Element -> Gpx
parseGpx gpxElem = 
   Gpx
   { 
      gpxRoute = parseRoute $ lookupChildElement1 (gpxName "rte") gpxElem
   }


elemAttr :: Name -> Element -> Text
elemAttr name elem = 
   let 
      msg = printf "Attribute '%s' not found." $ show name
   in
      fromMaybe (error msg) $ M.lookup name $ elementAttributes elem
      
      
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
