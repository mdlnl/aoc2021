import Data.Maybe
import qualified Data.Map as Map
import Split
import System.IO

parseInput input = (template, Map.fromList $ map parseInsertionLine insertionLines)
    where (template : _ : insertionLines) = nlsplit input

parseInsertionLine line = ((a,b), c)
    where [[a, b], [c]] = split " -> " line

insert :: Map.Map (Char, Char) Char -> String -> String
insert insertions [] = []
insert insertions [a] = [a]
insert insertions (a:b:s)
    | Map.member (a, b) insertions = a : (fromJust ins) : insert insertions (b:s)
    | otherwise                    = a :                  insert insertions (b:s)
    where ins = Map.lookup (a, b) insertions

times 0 f x = x
times n f x = f $ times (n-1) f x

hist [] = Map.fromList []
hist (x:xs) = Map.insert x (n+1) h
    where h = hist xs
          n = Map.findWithDefault 0 x h

part12 :: String -> Int -> Int
part12 input nsteps = mostCommon - leastCommon
    where (template, insertions) = parseInput input
          after10 = times nsteps (insert insertions) template
          counts = Map.elems $ hist after10
          leastCommon = minimum counts
          mostCommon = maximum counts

main = do
    input <- readFile "sample.txt"
    putStrLn $ "part1(sample) " ++ (show $ part12 input 10)
    input <- readFile "full.txt"
    putStrLn $ "part1(full) " ++ (show $ part12 input 10)
    input <- readFile "sample.txt"
    putStrLn $ "part2(sample) " ++ (show $ part12 input 40)
    input <- readFile "full.txt"
    putStrLn $ "part2(full) " ++ (show $ part12 input 40)
