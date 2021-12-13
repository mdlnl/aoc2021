import Data.List (intercalate)
import Data.Set (Set, elems, member, fromList)
import qualified Data.Set as Set (map)
import Debug.Trace
import Data.Tuple (fst, snd)
import Split
import System.IO

type Point = (Int, Int)

type Fold = Set Point -> Set Point

parsePoint :: String -> Point
parsePoint line = (read x :: Int, read y :: Int)
    where [x, y] = split "," line

-- parseFold "fold along x=5" returns a function that applies that fold to a set of points
parseFold :: String -> Fold
parseFold line
    | reverse prefix !! 0 == 'x' = Set.map (\(x, y) -> (fold x, y))
    | reverse prefix !! 0 == 'y' = Set.map (\(x, y) -> (x, fold y))
    where [prefix, axis] = split "=" line
          a = read axis :: Int
          fold u
            | u > a     = a - (u - a)
            | otherwise = u

parseInput :: String -> (Set Point, [Fold])
parseInput input = (fromList points, folds)
    where lines = nlsplit input
          points = map parsePoint $ takeWhile (/="") lines
          (_:folds) = map parseFold $ dropWhile (/="") lines

showPaper :: Set Point -> String
showPaper points = intercalate "\n" [
        [ pointAt (x, y) | x <- [0..n] ]
                         | y <- [0..m] ]
    where m = maximum [snd p | p <- elems points]
          n = maximum [fst p | p <- elems points]
          pointAt p
            | member p points = '#'
            | otherwise       = '.'

run filename = do
    input <- readFile filename
    let (points, folds) = parseInput input
    let allFolds = foldr (.) id $ reverse folds
    putStrLn $ filename ++ " after folds"
    putStrLn $ showPaper $ allFolds points

main = do
    run "sample.txt"
    run "full.txt"
