module Grid (Location, Grid, makeGrid, dijkstra) where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map (singleton, insert)
import Split

type Location = (Int, Int)

data Grid a = Grid Int Int (Location -> a)

instance Show a => Show (Grid a) where
    show (Grid m n g) = intercalate "\n" $ [intercalate "" [show $ g (i, j) | j <- [0..n-1]] | i <- [0..m-1]]

makeGrid str = Grid rows cols accessor
    where lines = nlsplit str
          rows = length lines
          cols = length $ lines !! 0
          accessor (i, j) = read [lines !! i !! j] :: Int

boundsCheck :: Int -> Int -> Location -> Bool
boundsCheck m n (i, j) = i >= 0 && j >= 0 && i < m && j < n

nsew :: Grid a -> Location -> [Location]
nsew (Grid m n _) (i, j) = filter (boundsCheck m n) [(i-1, j), (i+1, j), (i, j+1), (i, j-1)]

isLocalMinimum grid@(Grid _ _ h) loc = all (\n -> h n > h loc) $ nsew grid loc

allLocalMinima grid@(Grid m n h) = filter (isLocalMinimum grid) (allLocations grid)

allLocations (Grid m n _) = [ (i, j) | i <- [0..m-1], j <- [0..n-1] ]

--------------
-- Dijkstra --

type Metric :: Location -> Location -> Int

day09metric (Grid _ _ g) _ b = g b

dijkstra :: Num a => Grid a -> Metric -> Location -> Map Location a
dijkstra g m start = dijkstraAux g m start (allLocations g) (Map.singleton start 0)

dijkstraAux :: Num a => Grid a -> Metric -> Location -> [Location] -> Map Location a -> Map Location a
dijkstraAux _ _ _ [] dist = dist
dijkstraAux grid m start (q:qs) dist = 

--------------------------
-- day09-specific stuff --

day09_bfs :: Grid Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
day09_bfs grid [] visited = visited
day09_bfs grid@(Grid _ _ g) (q@(qi, qj):queue) visited
    | g q == 9  = day09_bfs grid queue visited
    | otherwise = q : day09_bfs grid newQueue (q : visited)
    where todo loc = not $ elem loc $ queue ++ visited
          newNeighbors = filter todo $ nsew grid q
          newQueue = queue ++ newNeighbors

day09_bfsFrom :: Grid Int -> (Int, Int) -> [(Int, Int)]
day09_bfsFrom grid start = day09_bfs grid [start] []
