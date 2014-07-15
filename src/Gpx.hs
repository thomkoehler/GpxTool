-----------------------------------------------------------------------------------------------------------------------

module Gpx
(
   Gpx(..),
   Route(..),
   RoutePoint(..),
   Extensions(..),
   RoutePointExtension(..),
   ExtensionRoutePoint(..)
)
where

-----------------------------------------------------------------------------------------------------------------------

import Data.Text(Text)

-----------------------------------------------------------------------------------------------------------------------

data Gpx = Gpx
   {
      gpxRoute :: [Route]
   }
   deriving(Show)


data Route = Route
   {
      rteName :: Maybe Text,
      rtePoints :: [RoutePoint]
   }
   deriving(Show)


data RoutePoint = RoutePoint
   {
      rteptLat :: Double,
      rteptLon :: Double,
      rteptExtensions :: Maybe Extensions 
   }
   deriving(Show)


data Extensions = Extensions
   {
      extRoutePointExtension :: RoutePointExtension
   } 
   deriving(Show)
   
   
data RoutePointExtension = RoutePointExtension
   {
      rpePoints :: [ExtensionRoutePoint] 
   }
   deriving(Show)


data ExtensionRoutePoint = ExtensionRoutePoint
   {
      erpLat :: Double,
      erpLon :: Double
   }
   deriving(Show)
   
  
-----------------------------------------------------------------------------------------------------------------------
