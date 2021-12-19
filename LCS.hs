module LCS (lcsWith) where

import Data.Map (Map, assocs, fromList, union, insert)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

lcs :: (Show a, Eq a) => [a] -> [a] -> [[a]]
lcs = lcsWith (==)

-- lcsWith eq x y returns the longest common subsequnces of x and y, where equality
-- is determined by eq
lcsWith :: Show a => (a -> a -> Bool) -> [a] -> [a] -> [[a]]
lcsWith _ [] _ = []
lcsWith _ _ [] = []
lcsWith eq x y = ij m n t
    where m = length x
          n = length y
          t = lcsWithDp (initial m n) 1 1 eq x y

type Table a = Map (Int, Int) [[a]]

ij i j t = fromJust $ Map.lookup (i, j) t

initial m n = union topRow leftCol
    where topRow = fromList [ ((0, j), [[]]) | j <- [0..m] ]
          leftCol = fromList [ ((i, 0), [[]]) | i <- [0..m] ]

lcsWithDp :: Show a => Table a -> Int -> Int -> (a -> a -> Bool) -> [a] -> [a] -> Table a
lcsWithDp dp i j eq x y
    | i > m     = dp
    | j > n     = lcsWithDp dp (i+1) 1 eq x y
    | otherwise = lcsWithDp nextdp i (j+1) eq x y
    where m = length x
          n = length y
          lcsij = compute dp i j eq x y
          nextdp = insert (i, j) lcsij dp

compute :: Show a => Table a -> Int -> Int -> (a -> a -> Bool) -> [a] -> [a] -> [[a]]
compute dp i j eq x y
    | eq xi yj  = ij (i-1) (j-1) dp
    | otherwise = --trace (show (assocs dp) ++ " " ++ show i ++ " " ++ show j) $
                  keepLongest above left
    where xi = x !! (i - 1)
          yj = y !! (j - 1)
          above = ij (i-1) j dp
          left  = ij i (j-1) dp

keepLongest :: [[a]] -> [[a]] -> [[a]]
keepLongest alist@(a:as) blist@(b:bs)
    | alen > blen = a : as
    | blen > alen = b : bs
    | otherwise   = alist ++ blist
    where alen = length a
          blen = length b