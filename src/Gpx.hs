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
   deriving(Show, Eq)


data Gpx = Gpx
   {
      gpxCreator :: Text,
      gpxVersion :: Text,
      gpxSchemaLocation :: Text,
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
   deriving(Show, Eq)


data Extensions = Extensions
   {
      extRoutePointExtension :: RoutePointExtension
   } 
   deriving(Show, Eq)
   
   
data RoutePointExtension = RoutePointExtension
   {
      rpePoints :: [Point] 
   }
   deriving(Show, Eq)
   
   
----------------------------------------------------------------------------------------------------------------------

instance Eq Gpx where
   (Gpx _ _ _ rts0) == (Gpx _ _ _ rts1) = rts0 == rts1
   
   
instance Eq Route where
   (Route _ pts0) == (Route _ pts1) = pts0 == pts1    


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
      _ <- printf "creator: %s\n" $ unpack $ gpxCreator gpx 
      _ <- printf "version: %s\n" $ unpack $ gpxVersion gpx
      _ <- printf "gpx contains %d route(s)\n" $ length $ gpxRoutes gpx 
      forM_ (gpxRoutes gpx) $ printInfo $ indent + 1


instance PrintInfo Route where
   printInfo indent rte@(Route name pts) = do
      replicateM_ indent $ putChar ' '
      let
         sizePoints = length pts
         sizeExtensionPoints = length (points rte) - sizePoints
         
      _ <- printf "route '%s' contains %d point(s)" (unpack (fromMaybe "" name)) sizePoints
      
      if sizeExtensionPoints == 0
         then putStrLn ""
         else printf " with %d extension points\n" sizeExtensionPoints
      
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
changeGpx routeNameChange routePointChange (Gpx _ ver schemaLocation routes) = 
   Gpx "GpxTool" ver schemaLocation $ map (changePoints .changeName) routes
      where
         changeName (Route n p) = Route (fmap routeNameChange n) p
         changePoints (Route n p) = Route n $ routePointChange p


reverseGpx :: Gpx -> Gpx
reverseGpx = changeGpx (`append` "_reverse") $ reverse . map reverseRoutePoint  


reverseRoutePoint :: RoutePoint -> RoutePoint
reverseRoutePoint rp@(RoutePoint _ Nothing) = rp
reverseRoutePoint rp = 
   let
      (pt : exPt) = reverse $ points rp 
   in
      RoutePoint
      {
         rteptPoint = pt,
         rteptExtensions = Just Extensions
            {
               extRoutePointExtension = RoutePointExtension
                  {
                     rpePoints = exPt
                  }
            }
      }
 

flattenGpx :: Gpx -> Gpx
flattenGpx = changeGpx (`append` "_flatten") $ concatMap flattenRoutePoint  
   
   
flattenRoutePoint :: RoutePoint -> [RoutePoint]
flattenRoutePoint (RoutePoint pt ext) = RoutePoint pt Nothing : map mkrp (points ext)
   where
      mkrp p = RoutePoint p Nothing
 
-----------------------------------------------------------------------------------------------------------------------