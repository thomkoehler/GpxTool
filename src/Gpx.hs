-----------------------------------------------------------------------------------------------------------------------

module Gpx
(
   Gpx(..),
   Route(..),
   RoutePoint(..),
   Extensions(..),
   RoutePointExtension(..),
   Point(..),
   reverseRoute
)
where

-----------------------------------------------------------------------------------------------------------------------

import Data.Text(Text)

-----------------------------------------------------------------------------------------------------------------------

data Point = Point
   {
      lat :: Double,
      lon :: Double
   } 
   deriving(Show)


class PointContainer pc where
   points :: pc -> [Point]


instance PointContainer pc => PointContainer (Maybe pc) where 
   points Nothing = []
   points (Just p) = points p


data Gpx = Gpx
   {
      gpxRoutes :: [Route]
   }
   deriving(Show)


data Route = Route
   {
      rteName :: Maybe Text,
      rtePoints :: [RoutePoint]
   }
   deriving(Show)

instance PointContainer Route where
   points (Route _ ps) = concatMap points ps
  

data RoutePoint = RoutePoint
   {
      rteptPoint :: Point,
      rteptExtensions :: Maybe Extensions 
   }
   deriving(Show)

instance PointContainer RoutePoint where
   points (RoutePoint pt ex) = pt : points ex 


data Extensions = Extensions
   {
      extRoutePointExtension :: RoutePointExtension
   } 
   deriving(Show)
   
instance PointContainer Extensions where
   points = points. extRoutePointExtension  
   
   
data RoutePointExtension = RoutePointExtension
   {
      rpePoints :: [Point] 
   }
   deriving(Show)
   
   
instance PointContainer RoutePointExtension where
   points = rpePoints  

-----------------------------------------------------------------------------------------------------------------------

reverseRoute :: Route -> Route
reverseRoute (Route name pts) = Route name $ reverse $ map reverseRoutePoint pts

reverseRoutePoint :: RoutePoint -> RoutePoint
reverseRoutePoint rp@(RoutePoint _ Nothing) = rp
reverseRoutePoint rp = 
   let
      (p : ps) = points rp
   in
      RoutePoint
      {
         rteptPoint = p,
         rteptExtensions = Just Extensions
            {
               extRoutePointExtension = RoutePointExtension
                  {
                     rpePoints = ps
                  }
            }
      }
    
 
-----------------------------------------------------------------------------------------------------------------------