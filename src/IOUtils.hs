module IOUtils
  ( writeField
  , cls
  , flush
  ) where

import Lib
import System.IO

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt pos text = do
  goto pos
  putStr text

cls :: IO ()
cls = putStr "\ESC[2J"

flush :: IO ()
flush = hFlush stdout

seqn :: [IO ()] -> IO ()
seqn [] = return ()
seqn (x:xs) = do
  x
  seqn xs

writeField :: String -> Field -> IO ()
writeField s f =
  seqn $ [writeAt pos s | pos <- board f] ++ [goto (w f + 1, h f + 1)]
