import Data.List (intercalate)
import Data.Map (Map, assocs, singleton, union)
import qualified Data.Map as Map
import Data.Maybe

import LCS
import Split
import Sort

data Scanner = Scanner Int Rotation [Vector]
instance Show Scanner where
    show (Scanner i _ bs) = "\nScanner " ++ show i ++ "\n" ++ (intercalate "\n" $ map show bs)

-------------
-- Parsing --

parse :: String -> [Scanner]
parse input = parseScanners $ nlsplit input

parseScanners :: [String] -> [Scanner]
parseScanners lines = map parseScanner $ split [""] lines

parseScanner :: [String] -> Scanner
parseScanner (titleRow:beaconRows) =
    Scanner (parseScannerTitleRow titleRow) id $ map parseBeaconPosition beaconRows

parseScannerTitleRow titleRow = read index
    where ["---", "scanner", index, "---"] = words titleRow

parseBeaconPosition line = V (read x) (read y) (read z)
    where [x,y,z] = split "," line

--------------
-- Geometry -- 

data Vector = V Int Int Int deriving (Eq, Show)

minus (V ux uy uz) (V vx vy vz) = V (ux-vx) (uy-vy) (uz-vz)

dot (V ux uy uz) (V vx vy vz) = (ux * vx) + (uy * vy) + (uz * vz)

mag2 u = dot u u

dist2 u v = mag2 $ minus u v

type Rotation = Vector -> Vector
rh1 (V x y z) = V x z (-y)
rh2 (V x y z) = V z y (-x)
rh3 (V x y z) = V (-y) x z
lh1 v = rh1 . rh1
lh2 v = rh2 . rh2
lh3 v = rh3 . rh3

pairwiseSquaredDistances beacons = [ dist2 u v | i <- [0..n-1], u <- [b i],
                                                 j <- [i+1..n-1], v <- [b j] ]
    where n = length beacons
          b = (beacons !!)

-----------------
-- Unification -- 

distancesPermitUnification (Scanner _ _ b1) (Scanner _ _ b2) =
        lcsLength psds1 psds2 >= 12
    where psds1 = quicksort id $ pairwiseSquaredDistances b1
          psds2 = quicksort id $ pairwiseSquaredDistances b2

unify (Scanner i1 r1 b1) (Scanner i2 r2 b2) = [] -- implement me

play filename = do
    input <- readFile filename
    let scanners = parse input
    putStrLn $ intercalate "\n" $ map show scanners