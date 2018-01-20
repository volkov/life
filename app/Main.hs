module Main where

import Lib
import IOUtils 
import Control.Concurrent

play :: Field -> IO ()
play f = do
  writeField "0" f
  flush
  threadDelay 500000
  writeField " " f
  play $ next f

someFunc :: IO ()
someFunc = do
  cls
  play glider

main :: IO ()
main = someFunc

