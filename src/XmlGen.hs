
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module XmlGen where

import qualified Data.Map as M
import Text.XML
import Text.Hamlet.XML

import Gpx

-----------------------------------------------------------------------------------------------------------------------

xmlGpx :: Gpx -> Element
xmlGpx gpx = Element "gpx" M.empty
   [xml|
      $forall route <- gpxRoutes gpx
         <rte>
            $maybe name <- rteName route 
               <name>{rteName route}
   |]

-----------------------------------------------------------------------------------------------------------------------
