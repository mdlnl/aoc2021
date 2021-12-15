import Data.Maybe
import Data.Map (Map, findWithDefault, (\\))
import qualified Data.Map as Map
import Debug.Trace
import Split
import System.IO

parseInput input = (template, Map.fromList $ map parseInsertionLine insertionLines)
    where (template : _ : insertionLines) = nlsplit input

type TwoChars = [Char]
type Rules = Map TwoChars Char

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

type Histogram = Map Char Int
type PairHistogram = Map TwoChars Int

zlookup :: Ord k => k -> Map k Int -> Int
zlookup = findWithDefault 0

-- make a histogram of letter-pairs
pairHist [] = Map.empty
pairHist [_] = Map.empty
pairHist (a:b:s) = Map.insert [a, b] (n + 1) ph
    where ph = pairHist $ b:s
          n = zlookup [a, b] ph

-- Apply insertion rules to the pairwise-histogram representation of the string.
-- Verison 1 -- apply each insertion to the relevant pairwise-histogram bin by removing the affected
--              pair and then updating the two new pairs.
phInsert :: Rules -> (PairHistogram, Histogram) -> (PairHistogram, Histogram)
phInsert ins phh = phInsertInc (Map.assocs ins) phh
phInsertInc :: [(TwoChars, Char)] -> (PairHistogram, Histogram) ->
               (PairHistogram, Histogram)
phInsertInc [] phh = phh
phInsertInc (([a, b], c) : insertions) (ph, h)
    | zlookup [a, b] ph > 0 = phInsertInc insertions (cbInc, Map.insertWith (+) c k h)
    | otherwise             = phInsertInc insertions (ph, h)
    where k = zlookup [a, b] ph -- the original pair
          abDec = Map.delete [a, b] ph
          acInc = Map.insertWith (+) [a, c] k abDec
          cbInc = Map.insertWith (+) [c, b] k acInc

-- Apply insertion rules to the pairwise-histogram representation of the string.
-- Version 2 -- intersect the rules with the current pairwise histogram, producing a todo list.
phInsertZipped :: Rules -> (PairHistogram, Histogram) -> (PairHistogram, Histogram)
phInsertZipped rules (ph, h) = (withOrig, newHist)
          -- todo list is a "zipped" histogram where each bin gets tagged with the letter to insert
    where todo      = Map.intersectionWith (,) ph rules
          unchanged = ph \\ todo
          -- new pairs resulting from the left and right halves of every pair after insertion.
          -- fromListWith (+) makes a pairwise histogram from a (k,v) list with duplicate keys by summing.
          left      = Map.fromListWith (+) [ ([a,c], n) | ([a,_], (n, c)) <- Map.assocs todo ] 
          right     = Map.fromListWith (+) [ ([c,b], n) | ([_,b], (n, c)) <- Map.assocs todo ]
          leftRight = Map.unionWith (+) left right
          withOrig  = Map.unionWith (+) unchanged leftRight
          -- Generate the new histogram by adding all the inserted letter's counts to the current histogram.
          newHist   = Map.foldr (\(n, c) -> Map.insertWith (+) c n) h todo
          
part2 :: String -> Int -> Int
part2 input nsteps = mostCommon - leastCommon
    where (template@(template0:_), insertions) = parseInput input
          (ph, h) = --trace ("\nfinal string " ++ times nsteps (insert insertions) template) $
                    times nsteps (phInsertZipped insertions) (pairHist template, hist template)
          leastCommon = --trace ("\nfinal ph " ++ show ph) $
                        minimum h
          mostCommon = --trace ("\nfinal h " ++ show h) $
                        maximum h

checkPairwiseAgainstBrute ins n template = Map.differenceWith diff pairwiseHist bruteHist
    where (pairwisePairHist, pairwiseHist) = times n (phInsertZipped ins) (pairHist template, hist template)
          templateN = times n (insert ins) template
          brutePairHist = pairHist templateN
          bruteHist = hist templateN
          diff pw b = if pw == b then Nothing else Just b

main = do
    input <- readFile "sample.txt"
    putStrLn $ "part1(sample) " ++ (show $ part1 input 10)
    input <- readFile "full.txt"
    putStrLn $ "part1(full) " ++ (show $ part1 input 10)
    input <- readFile "sample.txt"
    putStrLn $ "part2(sample) " ++ (show $ part2 input 40)
    input <- readFile "full.txt"
    putStrLn $ "part2(full) " ++ (show $ part2 input 40)