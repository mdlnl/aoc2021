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

phInsertZipped :: Rules -> (PairHistogram, Histogram) -> (PairHistogram, Histogram)
phInsertZipped ins (ph, h) = (
            Map.unionWith (+) unchanged $ Map.unionWith (+) left right,
            Map.foldr (\(n, c) -> Map.insertWith (+) c n) h todo
    )
    where todo      = Map.intersectionWith (,) ph ins
          unchanged = ph \\ todo
          left      = Map.fromList [ ([a,c], n) | ([a,_], (n, c)) <- Map.assocs todo ] 
          right     = Map.fromList [ ([c,b], n) | ([_,b], (n, c)) <- Map.assocs todo ]
          
part2 :: String -> Int -> Int
part2 input nsteps = mostCommon - leastCommon
    where (template@(template0:_), insertions) = parseInput input
          (ph, h) = trace ("\nfinal string " ++ times nsteps (insert insertions) template) $
                    (phInsertZipped insertions) (pairHist template, hist template)
          leastCommon = trace ("\nfinal ph " ++ show ph) $ minimum h
          mostCommon = trace ("\nfinal h " ++ show h) $ maximum h

checkPairwiseAgainstBrute ins n template = pairwise == brute
    where pairwise = times n (phInsertZipped ins) (pairHist template, hist template)
          templateN = times n (insert ins) template
          brute = (pairHist templateN, hist templateN)

main = do
    input <- readFile "sample.txt"
    putStrLn $ "part1(sample) " ++ (show $ part1 input 10)
    input <- readFile "full.txt"
    putStrLn $ "part1(full) " ++ (show $ part1 input 10)
    input <- readFile "sample.txt"
    putStrLn $ "part2(sample) " ++ (show $ part2 input 10)
    --input <- readFile "full.txt"
    --putStrLn $ "part2(full) " ++ (show $ part2 input 10)
