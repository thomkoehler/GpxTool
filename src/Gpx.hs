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
import Data.Data

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
   deriving(Show, Data, Typeable)
   

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
   deriving(Show, Data, Typeable)
   
   
data RoutePointExtension = RoutePointExtension
   {
      rpePoints :: [Point] 
   }
   deriving(Show, Data, Typeable)

-----------------------------------------------------------------------------------------------------------------------
