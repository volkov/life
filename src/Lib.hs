module Lib
  ( next
  , glider
  , width
  , height
  , Board
  , Pos 
  ) where

import Control.Concurrent
import System.IO

type Pos = (Int, Int)

type Board = [Pos]

isAlive :: Pos -> Board -> Bool
isAlive pos board = elem pos board

width :: Int
width = 5

height :: Int
height = 5

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

neighbours :: Pos -> [Pos]
neighbours (x, y) =
  map
    wrap
    [ (x - 1, y - 1)
    , (x, y - 1)
    , (x + 1, y - 1)
    , (x - 1, y)
    , (x + 1, y)
    , (x - 1, y + 1)
    , (x, y + 1)
    , (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) = ((x - 1) `mod` width + 1, (y - 1) `mod` height + 1)

aliveNeighbours :: Board -> Pos -> Int
aliveNeighbours board = length . filter (`isAlive` board) . neighbours

survivors :: Board -> [Pos]
survivors board = [x | x <- board, aliveNeighbours board x `elem` [2, 3]]

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : (rmDups $ filter (/= x) xs)

births :: Board -> [Pos]
births board =
  [ x
  | x <- rmDups $ concat $ map neighbours board
  , not (isAlive x board)
  , aliveNeighbours board x == 3
  ]

next :: Board -> Board
next board = rmDups $ concat [births board, survivors board]


