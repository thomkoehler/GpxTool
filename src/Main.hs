
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Text.XML
import Filesystem.Path.CurrentOS(fromText)
import qualified Data.ByteString.Lazy as B
import System.IO(stdout)
import System.Environment(getArgs)

import XmlParser
import XmlGen
import Options


main :: IO ()
main = do
   args <- getArgs
   (ops, inFile) <- options args
   (route, doc) <- parseFile $ fromText "Route1.gpx"
   let bs = renderGpx True (documentPrologue doc) (documentEpilogue doc) route
   B.hPut stdout bs
   return ()

   
