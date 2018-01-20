module IOUtils
  ( writeBoard
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

writeBoard :: String -> Board -> IO ()
writeBoard s board =
  seqn $ [writeAt pos s | pos <- board] ++ [goto (width + 1, height + 1)]
