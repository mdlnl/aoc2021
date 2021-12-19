module LCS (lcsWith) where

import Data.Map (Map, fromList, union)
import qualified Data.Map as Map

-- lcsWith eq as bs returns the longest common subsequnces of as and bs, where equality
-- is determined by eq
lcsWith :: (a -> a -> Bool) -> [a] -> [a] -> [[a]]
lcsWith _ [] _ = []
lcsWith _ _ [] = []
lcsWith eq as bs = lcsWithDp (initial m n) 1 1 eq as bs
    where m = length as
          n = length bs

type Table a = Map (Int, Int) [[a]]

initial m n = union topRow leftCol
    where topRow = fromList [ ((0, j), [[]]) | j <- [0..m] ]
          leftCol = fromList [ ((i, 0), [[]]) | i <- [0..m] ]

lcsWithDp :: Table a -> Int -> Int -> (a -> a -> Bool) -> [a] -> [a] -> [[a]]
lcsWithDp table i j eq as bs = []
