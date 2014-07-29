
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
      gpxSchemaLocation = "",
      gpxRoutes = 
      [
         Route
         {
            rteName = Nothing,
            rtePoints = routePoints 
         }
      ]
   }
   
createRoutePointExtension :: [Point] -> Maybe Extensions
createRoutePointExtension points = Just Extensions
   {
      extRoutePointExtension = RoutePointExtension
         {
            rpePoints = points
         }
   } 

exGpx :: Gpx
exGpx = createTestGpx
   [
      RoutePoint (Point 1.0 2.0) (createRoutePointExtension [Point 3.0 4.0, Point 5.0 6.0]),
      RoutePoint (Point 7.0 8.0) (createRoutePointExtension [Point 9.0 10.0, Point 11.0 12.0])
   ]
   
exGpx' :: Gpx
exGpx' = createTestGpx
   [
      RoutePoint (Point 1.0 2.0) (createRoutePointExtension [Point 3.0 4.0, Point 5.0 6.0]),
      RoutePoint (Point 7.0 8.0) (createRoutePointExtension [Point 9.0 10.0, Point 11.0 12.0])
   ]

exGpxReverse :: Gpx
exGpxReverse = createTestGpx
   [
      RoutePoint (Point 11.0 12.0) (createRoutePointExtension [Point 9.0 10.0, Point 7.0 8.0]),
      RoutePoint (Point 5.0 6.0) (createRoutePointExtension [Point 3.0 4.0, Point 1.0 2.0])
   ]

simpleGpx :: Gpx
simpleGpx = createTestGpx
   [
      RoutePoint (Point 1.0 2.0) Nothing,
      RoutePoint (Point 3.0 4.0) Nothing
   ]
   
   
simpleGpx' :: Gpx
simpleGpx' = createTestGpx
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

prop_neqGpx :: Bool
prop_neqGpx = simpleGpx /= simpleGpxReverse

prop_eqGpx :: Bool
prop_eqGpx = simpleGpx == simpleGpx'

prop_simpleReverse0 :: Bool
prop_simpleReverse0 = reverseGpx simpleGpx == simpleGpxReverse

prop_simpleReverse1 :: Bool
prop_simpleReverse1 = reverseGpx simpleGpxReverse == simpleGpx

prop_simpleReverseDouble :: Bool
prop_simpleReverseDouble = reverseGpx (reverseGpx simpleGpx) == simpleGpx

prop_neqExGpx :: Bool
prop_neqExGpx = exGpx /= exGpxReverse

prop_eqExGpx :: Bool
prop_eqExGpx = exGpx == exGpx'

prop_exReverse0 :: Bool
prop_exReverse0 = reverseGpx exGpx == exGpxReverse

prop_exReverse1 :: Bool
prop_exReverse1 = reverseGpx exGpxReverse == exGpx

prop_exReverseDouble :: Bool
prop_exReverseDouble = reverseGpx (reverseGpx exGpx) == exGpx

-----------------------------------------------------------------------------------------------------------------------
