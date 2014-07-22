-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Text.XML
import Data.Text.Internal()
import Data.String
import System.Environment(getArgs)
import Control.Monad(when)
import System.FilePath(takeBaseName, replaceBaseName)
import Text.Printf

import XmlParser
import XmlGen
import Options
import Gpx

-----------------------------------------------------------------------------------------------------------------------
 
main :: IO ()
main = do
   args <- getArgs
   let (ops, inFile) = options args
   when (Reverse `elem` ops) $ 
      transformFile reverseGpx inFile $ appendBaseName "_reverse" inFile
      
   when (Flatten `elem` ops) $
      transformFile reverseGpx inFile $ appendBaseName "_flatten" inFile
      
      
   when (Info `elem` ops) $ do
      (gpx, _) <- parseFile $ fromString inFile
      printGpxInfo gpx
      
      
   return ()


transformFile :: (Gpx -> Gpx) -> String -> String -> IO ()
transformFile transFun inFile outFile = do
   (gpx, doc) <- parseFile $ fromString inFile
   let tgpx = transFun gpx
   writeGpx False (documentPrologue doc) (documentEpilogue doc) (fromString outFile) tgpx
   _ <- printf "File '%s' created." outFile
   return ()


appendBaseName :: String -> String -> String
appendBaseName postfix baseName = replaceBaseName baseName $ takeBaseName baseName ++ postfix 

-----------------------------------------------------------------------------------------------------------------------
