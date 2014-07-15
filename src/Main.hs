
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import qualified Data.Map as M
import Prelude hiding (readFile, writeFile)
import Text.XML
import Data.Text(Text)
import Filesystem.Path.CurrentOS
import Text.Printf
import Data.Maybe



data Gpx = Gpx
   {
      gpxRoute :: Route
   }
   deriving(Show)

data Route = Route
   {
      rteName :: Text
   }
   deriving(Show)

main :: IO ()
main = do
   Document prologue root epilogue <- readFile def $ fromText "Route1.gpx"
   print $ parseGpx root
   return ()

   
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

   