
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module XmlGen(renderGpx) where

import qualified Data.Map as M
import Text.XML
import Text.Hamlet.XML
import qualified Data.ByteString.Lazy as B

import Gpx

-----------------------------------------------------------------------------------------------------------------------

xmlGpx :: Gpx -> Element
xmlGpx gpx = Element "gpx" M.empty
   [xml|
      $forall route <- gpxRoutes gpx
         <rte>
            $maybe name <- rteName route 
               <name>#{name}
   |]


renderGpx :: Prologue -> [Miscellaneous] -> Gpx -> B.ByteString
renderGpx prologue epilogue gpx = renderLBS def (Document prologue elem epilogue)
   where 
      elem = xmlGpx gpx


-----------------------------------------------------------------------------------------------------------------------
