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


class PrintInfo pi where
   printInfo :: Int -> pi -> IO ()


data Gpx = Gpx
   {
      gpxRoutes :: [Route]
   }
   deriving(Show)


instance PrintInfo Gpx where
   printInfo indent gpx = do
      replicateM_ indent $ putChar ' '
      _ <- printf "gpx contains %d route(s)\n" $ length $ gpxRoutes gpx 
      forM_ (gpxRoutes gpx) $ printInfo 1


data Route = Route
   {
      rteName :: Maybe Text,
      rtePoints :: [RoutePoint]
   }
   deriving(Show)


instance PointContainer Route where
   points (Route _ ps) = concatMap points ps

  
instance PrintInfo Route where
   printInfo indent (Route name _) = do
      replicateM_ indent $ putChar ' '
      _ <- printf "route name '%s'\n" $ unpack $ fromMaybe "" name
      return ()
  

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