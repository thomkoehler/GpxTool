-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Text.XML
import Filesystem.Path.CurrentOS(fromText)
import System.Environment(getArgs)
import Control.Monad(when)
import System.FilePath(takeBaseName, replaceBaseName)

import XmlParser
import XmlGen
import Options
import Gpx

-----------------------------------------------------------------------------------------------------------------------
 
main :: IO ()
main = do
   args <- getArgs
   let (ops, inFile) = options args
   when (elem Reverse ops) $ reverseFile inFile $ appendBaseName "_reverse" inFile
   return ()


reverseFile :: String -> String -> IO ()
reverseFile inFile outFile = do
   (route, doc) <- parseFile $ fromText $ read inFile
   let rr = reverseGpx route
   writeGpx False (documentPrologue doc) (documentEpilogue doc) (fromText (read outFile)) rr
   return ()


appendBaseName :: String -> String -> String
appendBaseName postfix baseName = replaceBaseName baseName $ (takeBaseName baseName) ++ postfix 

-----------------------------------------------------------------------------------------------------------------------
