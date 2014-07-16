
{-# LANGUAGE OverloadedStrings #-}

module Main where

import XmlParser
import XmlGen

import Filesystem.Path.CurrentOS(fromText)


main :: IO ()
main = do
   route <- parseFile $ fromText "Route1.gpx"
   print $ xmlGpx route
   return ()

   
