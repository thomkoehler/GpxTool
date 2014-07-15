-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module XmlParser where

import Prelude hiding(FilePath, readFile)
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import Text.XML
import Text.Printf(printf)
import Filesystem.Path.CurrentOS(FilePath)

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
parseRoute = Route . elemAttr (gpxName "name")

parseGpx :: Element -> Gpx
parseGpx = Gpx . parseRoute . lookupElement1 (gpxName "rte")


elemAttr :: Name -> Element -> Text
elemAttr name elem = 
   let 
      msg = printf "Attribute '%s' not found." $ show name
   in
      fromMaybe (error msg) $ M.lookup name $ elementAttributes elem
      
      
lookupElements :: Name -> Element -> [Element]
lookupElements name elem =
   map nodeToElement $ filter filterFun $ elementNodes elem
   where
      nodeToElement (NodeElement childElem) = childElem
   
      filterFun (NodeElement childElem) = elementName childElem == name
      filterFun _ = False

      
lookupElement1 :: Name -> Element -> Element
lookupElement1 name elem = case lookupElements name elem of
   []      -> error $ printf "Element %s not found." $ show name
   [child] -> child
   _       -> error $ printf "More than one Element %s found." $ show name

   

-----------------------------------------------------------------------------------------------------------------------
