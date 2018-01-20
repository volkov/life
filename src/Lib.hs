module Lib
  ( next
  , glider
  , w
  , h
  , board
  , Board
  , Pos
  , Field 
  ) where

import Control.Concurrent
import System.IO

type Pos = (Int, Int)

type Board = [Pos]

data Field = Field { board :: Board
                   , w     :: Int
                   , h     :: Int
                   }

isAlive :: Pos -> Board -> Bool
isAlive pos board = elem pos board

glider :: Field 
glider = Field { board  = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]
               , w  = 10
               , h = 10
               }

neighbours :: Int -> Int -> Pos -> [Pos]
neighbours w h (x, y) =
  map
    (wrap w h)
    [ (x - 1, y - 1)
    , (x, y - 1)
    , (x + 1, y - 1)
    , (x - 1, y)
    , (x + 1, y)
    , (x - 1, y + 1)
    , (x, y + 1)
    , (x + 1, y + 1)
    ]

wrap :: Int -> Int -> Pos -> Pos
wrap w h (x, y) = ((x - 1) `mod` w + 1, (y - 1) `mod` h + 1)

aliveNeighbours :: Field -> Pos -> Int
aliveNeighbours field = length . filter (`isAlive` (board field)) . (neighbours (w field) (h field))

survivors :: Field -> [Pos]
survivors field = [x | x <- board field, aliveNeighbours field x `elem` [2, 3]]

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : (rmDups $ filter (/= x) xs)

births :: Field -> [Pos]
births field =
  [ x
    | x <- rmDups $ concat $ map (neighbours (w field) (h field)) (board field)
  , not (isAlive x $ board field)
  , aliveNeighbours field x == 3
  ]

next :: Field -> Field 
next field = Field{ board = rmDups $ concat [births field, survivors field]
                  , w = w field
                  , h = h field
                  }


