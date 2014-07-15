
module Main where

import XmlParser


main :: IO ()
main = do
   route <- parseFile "Route.gpx"
   print route
   return ()

   
