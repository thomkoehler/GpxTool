
{-# LANGUAGE OverloadedStrings #-}

module Main where

import XmlParser

import Filesystem.Path.CurrentOS(fromText)

main :: IO ()
main = do
   route <- parseFile $ fromText "Route1.gpx"
   print route
   return ()

   
