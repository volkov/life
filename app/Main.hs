module Main where

import Lib
import IOUtils 
import Control.Concurrent

play :: Board -> IO ()
play b = do
  writeBoard "0" b
  flush
  threadDelay 500000
  writeBoard " " b
  play $ next b

someFunc :: IO ()
someFunc = do
  cls
  play glider

main :: IO ()
main = someFunc

