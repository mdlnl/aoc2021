module LCS (lcs, lcsWith) where

import Data.Map (Map, assocs)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set, empty)
import qualified Data.Set as Set
import Debug.Trace

lcs :: (Ord a, Eq a) => [a] -> [a] -> Set [a]
lcs = lcsWith (==) (\a _ -> a)

-- lcsWith eq com x y returns the longest common subsequnces of x and y, where equality
-- is determined by eq, and "equal" elements are combined using com
lcsWith :: (Ord a) => (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> Set [a]
lcsWith _ _ [] _ = empty
lcsWith _ _ _ [] = empty
lcsWith eq com x y = lcsWithDp (initial m n) 1 1 eq com x y
    where m = length x
          n = length y

type Table a = Map (Int, Int) (Set [a])

ij i j t = fromJust $ Map.lookup (i, j) t

emptyPrefix = Set.singleton []

initial m n = Map.union topRow leftCol
    where topRow  = Map.fromList [ ((0, j), emptyPrefix) | j <- [0..m] ]
          leftCol = Map.fromList [ ((i, 0), emptyPrefix) | i <- [0..m] ]

lcsWithDp :: (Ord a) => Table a -> Int -> Int -> (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> Set [a]
lcsWithDp dp i j eq com x y
    | i > m     = ij i j dp
    | j > n     = lcsWithDp dp (i+1) 1 eq com x y
    | otherwise = lcsWithDp nextdp i (j+1) eq com x y
    where m = length x
          n = length y
          lcsij = compute dp i j eq com x y
          nextdp = Map.insert (i, j) lcsij dp

compute :: (Ord a) => Table a -> Int -> Int -> (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> Set [a]
compute dp i j eq com x y
    | eq xi yj  = Set.map (++[com xi yj]) $ ij (i-1) (j-1) dp
    | otherwise = keepLongest above left
    where xi = x !! (i - 1)
          yj = y !! (j - 1)
          above = ij (i-1) j dp
          left  = ij i (j-1) dp

keepLongest :: Ord a => Set [a] -> Set [a] -> Set [a]
keepLongest as bs
    | alen > blen = as
    | blen > alen = bs
    | otherwise   = Set.union as bs
    where alen = length $ (Set.toList as) !! 0
          blen = length $ (Set.toList bs) !! 0