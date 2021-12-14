import Data.Maybe
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map
import Debug.Trace
import Split
import System.IO

parseInput input = (template, Map.fromList $ map parseInsertionLine insertionLines)
    where (template : _ : insertionLines) = nlsplit input

type TwoChars = [Char]

parseInsertionLine :: String -> (TwoChars, Char)
parseInsertionLine line = ([a,b], c)
    where [[a, b], [c]] = split " -> " line

insert :: Map TwoChars Char -> String -> String
insert insertions [] = []
insert insertions [a] = [a]
insert insertions (a:b:s)
    | Map.member [a, b] insertions = a : (fromJust ins) : insert insertions (b:s)
    | otherwise                    = a :                  insert insertions (b:s)
    where ins = Map.lookup [a, b] insertions

times 0 f x = x
times n f x = f $ times (n-1) f x

hist :: Ord a => [a] -> Map a Int
hist [] = Map.fromList []
hist (x:xs) = Map.insert x (n+1) h
    where h = hist xs
          n = zlookup x h

part1 :: String -> Int -> Int
part1 input nsteps = mostCommon - leastCommon
    where (template, insertions) = parseInput input
          afterNsteps = times nsteps (insert insertions) template :: String
          counts = Map.elems $ hist afterNsteps :: [Int]
          leastCommon = minimum counts
          mostCommon = maximum counts

type PairHistogram = Map TwoChars Int

zlookup :: Ord k => k -> Map k Int -> Int
zlookup = findWithDefault 0

-- make a histogram of letter-pairs
pairHist [] = Map.empty
pairHist [_] = Map.empty
pairHist (a:b:s) = Map.insert [a, b] (n + 1) ph
    where ph = pairHist $ b:s
          n = zlookup [a, b] ph

phInsert :: [(TwoChars, Char)] -> PairHistogram -> PairHistogram
phInsert [] ph = ph 
phInsert (([a, b], c) : insertions) ph = phInsert insertions cbInc
    where ab = zlookup [a, b] ph -- the original pair
          abDec = Map.insert [a, b] 0 ph
          acInc = Map.insertWith (+) [a, c] ab abDec
          cbInc = Map.insertWith (+) [c, b] ab acInc
          
part2 :: String -> Int -> Int
part2 input nsteps = mostCommon - leastCommon
    where (template@(template0:_), insertions) = parseInput input
          afterNsteps = trace (times nsteps (insert insertions) template) $
                        (phInsert $ Map.assocs insertions) $ pairHist template
          -- A letter may occur in many pairs. We need to combine the counts for each pair.
          letterHist = trace (show afterNsteps) $
            foldr (
                -- count only second letters in pairs. This leaves out the first letter in the final
                -- string, which is also the first letter in the template, so that's our starting
                -- point for foldr.
                -- The updater takes a list element (pairHist key-value pair) and an existing
                -- single-letter histogram, and returns the new histogram.
                \([_,b], n) h -> Map.insertWith (+) b n h
            ) (Map.singleton template0 1) (Map.assocs afterNsteps)
          counts = trace (show letterHist) $ Map.elems $ letterHist
          leastCommon = minimum counts
          mostCommon = maximum counts

main = do
    input <- readFile "sample.txt"
    putStrLn $ "part1(sample) " ++ (show $ part1 input 10)
    input <- readFile "full.txt"
    putStrLn $ "part1(full) " ++ (show $ part1 input 10)
    input <- readFile "sample.txt"
    putStrLn $ "part2(sample) " ++ (show $ part2 input 1)
    --input <- readFile "full.txt"
    --putStrLn $ "part2(full) " ++ (show $ part2 input 10)
