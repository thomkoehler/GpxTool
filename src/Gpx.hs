-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Gpx
(
   Gpx(..),
   Route(..),
   RoutePoint(..),
   Extensions(..),
   RoutePointExtension(..),
   Point(..)
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


data Point = Point
   {
      lat :: Double,
      lon :: Double
   } 
   deriving(Show)
   

data RoutePoint = RoutePoint
   {
      rteptPoint :: Point,
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
      rpePoints :: [Point] 
   }
   deriving(Show)

-----------------------------------------------------------------------------------------------------------------------

class PointContainer pc where
   getPoints :: pc -> [Point]
   
instance PointContainer RoutePointExtension where
   getPoints = rpePoints
   
instance PointContainer Extensions where
   getPoints = getPoints . extRoutePointExtension

flattenPoints :: RoutePoint -> RoutePoint
flattenPoints (RoutePoint ps ex) = 
   let
      allPoints = ps : getPoints ex
   in
      RoutePoint { rteptPoint = allPoints, rteptExtensions = ex }

