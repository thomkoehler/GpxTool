
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module XmlGen(renderGpx, writeGpx) where

import Prelude hiding(writeFile, FilePath)

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Text.XML
import Text.Hamlet.XML
import Data.Text(pack)
import Filesystem.Path(FilePath)

import Gpx

-----------------------------------------------------------------------------------------------------------------------

xmlGpx :: Gpx -> Element
xmlGpx gpx = Element "gpx" (M.fromList [("creator", gpxCreator gpx), ("version", gpxVersion gpx)])
   [xml|
      $forall route <- gpxRoutes gpx
         <rte>
            $maybe name <- rteName route 
               <name>#{name}
            $forall point <- rtePoints route 
               <rtept lat=#{pack (show (lat (rteptPoint point)))} lon=#{pack (show (lon (rteptPoint point)))}>
                  $maybe ext <- rteptExtensions point
                     <extensions>
                        <gpxx:RoutePointExtension>
                           $forall point <- rpePoints (extRoutePointExtension ext)
                              <gpxx:rpt lat=#{pack (show (lat point))} lon=#{pack (show (lon point))}>
   |]


renderGpx :: Bool -> Prologue -> [Miscellaneous] -> Gpx -> B.ByteString
renderGpx pretty prologue epilogue gpx = renderLBS settings (Document prologue elems epilogue)
   where
      elems = xmlGpx gpx
      settings = def { rsPretty = pretty}
      
      
writeGpx :: Bool -> Prologue -> [Miscellaneous] -> FilePath -> Gpx -> IO ()
writeGpx pretty prologue epilogue path gpx = writeFile settings path $ Document prologue elems epilogue
   where
      elems = xmlGpx gpx
      settings = def { rsPretty = pretty}

-----------------------------------------------------------------------------------------------------------------------
