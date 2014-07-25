
-----------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Test.Framework

import Gpx

-----------------------------------------------------------------------------------------------------------------------

main :: IO()
main = htfMain htf_thisModulesTests

-----------------------------------------------------------------------------------------------------------------------

createTestGpx :: [RoutePoint] -> Gpx
createTestGpx routePoints = Gpx
   {
      gpxCreator = "Test",
      gpxVersion = "1.1",
      gpxRoutes = 
      [
         Route
         {
            rteName = Nothing,
            rtePoints = routePoints 
         }
      ]
   }
   

simpleGpx :: Gpx
simpleGpx = createTestGpx
   [
      RoutePoint (Point 1.0 2.0) Nothing,
      RoutePoint (Point 3.0 4.0) Nothing
   ]
   
   
simpleGpxReverse :: Gpx
simpleGpxReverse = createTestGpx
   [
      RoutePoint (Point 3.0 4.0) Nothing,
      RoutePoint (Point 1.0 2.0) Nothing
   ]

-----------------------------------------------------------------------------------------------------------------------

prop_SimpleReverse0 :: Bool
prop_SimpleReverse0 = reverseGpx simpleGpx == simpleGpxReverse


prop_SimpleReverse1 :: Bool
prop_SimpleReverse1 = reverseGpx simpleGpxReverse == simpleGpx

-----------------------------------------------------------------------------------------------------------------------
