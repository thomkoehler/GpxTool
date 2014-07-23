-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Gpx
(
   Gpx(..),
   Route(..),
   RoutePoint(..),
   Extensions(..),
   RoutePointExtension(..),
   Point(..),
   printGpxInfo,
   reverseGpx,
   flattenGpx
)
where

-----------------------------------------------------------------------------------------------------------------------

import Control.Monad(replicateM_, forM_)
import Text.Printf(printf)
import Data.Maybe(fromMaybe)
import Data.Text(unpack, Text, append)

-----------------------------------------------------------------------------------------------------------------------

class PointContainer pc where
   points :: pc -> [Point]


class PrintInfo pi where
   printInfo :: Int -> pi -> IO ()


data Point = Point
   {
      lat :: Double,
      lon :: Double
   } 
   deriving(Show)


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
   
   
----------------------------------------------------------------------------------------------------------------------

instance PointContainer pc => PointContainer (Maybe pc) where 
   points Nothing = []
   points (Just p) = points p


instance PointContainer Route where
   points (Route _ ps) = concatMap points ps


instance PointContainer RoutePoint where
   points (RoutePoint pt ex) = pt : points ex   


instance PointContainer Extensions where
   points = points. extRoutePointExtension  


instance PointContainer RoutePointExtension where
   points = rpePoints  


instance PrintInfo Gpx where
   printInfo indent gpx = do
      replicateM_ indent $ putChar ' '
      _ <- printf "gpx contains %d route(s)\n" $ length $ gpxRoutes gpx 
      forM_ (gpxRoutes gpx) $ printInfo $ indent + 1


instance PrintInfo Route where
   printInfo indent (Route name pts) = do
      replicateM_ indent $ putChar ' '
      _ <- printf "route name '%s' contains %d point(s)\n" (unpack (fromMaybe "" name)) $ length pts
      forM_ pts $ printInfo $ indent + 1
      return ()


instance PrintInfo RoutePoint where
   printInfo indent (RoutePoint point _) = do
      replicateM_ indent $ putChar ' '
      _ <- printf "point: lat=%f lon=%f\n" (lat point) (lon point)
      return ()


printGpxInfo :: Gpx -> IO ()
printGpxInfo = printInfo 0

changeGpx :: (Text -> Text) -> ([RoutePoint] -> [RoutePoint]) -> Gpx -> Gpx
changeGpx routeNameChange routePointChange (Gpx routes) = Gpx $ map (changePoints .changeName) routes
   where
      changeName (Route n p) = Route (fmap routeNameChange n) p
      changePoints (Route n p) = Route n $ routePointChange p


reverseGpx :: Gpx -> Gpx
reverseGpx = changeGpx (append "_reverse") $ map reverseRoutePoint  


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
 

flattenGpx :: Gpx -> Gpx
flattenGpx = changeGpx (append "_flatten") $ concatMap flattenRoutePoint  
   
   
flattenRoutePoint :: RoutePoint -> [RoutePoint]
flattenRoutePoint (RoutePoint pt ext) = RoutePoint pt Nothing : map mkrp (points ext)
   where
      mkrp p = RoutePoint p Nothing
 
-----------------------------------------------------------------------------------------------------------------------