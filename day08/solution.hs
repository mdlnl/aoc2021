import Data.List (intercalate)
import Data.Set (Set, size, fromList, isSubsetOf, singleton, difference, (\\), elems)
import Debug.Trace
import System.IO

isPrefix [] _ = True
isPrefix (p:ps) [] = False
isPrefix (p:ps) (x:xs) = p == x && isPrefix ps xs

splitAtDelim delim [] = ([], [])
splitAtDelim delim list@(x:xs)
        | isPrefix delim list = ([], drop (length delim) list)
        | otherwise = (x : before, after)
        where (before, after) = splitAtDelim delim xs

split _ [] = []
split delim list = before : split delim after
        where (before, after) = splitAtDelim delim list

data Entry = Entry [String] [String] deriving (Show)

nlsplit = split "\n"

barsplit line = Entry (words before) (words after)
        where (before, after) = splitAtDelim " | " line

only :: (Show a) => [a] -> a
only [] = error "empty list"
only [x] = x
only xs = error $ "only expected only one element but got " ++ (show xs)

uniqueSize :: (Show a) =>  Int -> [Set a] -> Set a
uniqueSize n list = only $ filter (\e -> n == (size e)) list

setFilter :: (Ord a) => (a -> Bool) -> Set a -> Set a
setFilter f s = fromList $ filter f $ elems s

solve :: [String] -> String -> Char
solve signalStrings = (\sigString -> case () of
                           _ | sigs == zero  -> '0'
                             | sigs == one   -> '1'
                             | sigs == two   -> '2'
                             | sigs == three -> '3'
                             | sigs == four  -> '4'
                             | sigs == five  -> '5'
                             | sigs == six   -> '6'
                             | sigs == seven -> '7'
                             | sigs == eight -> '8'
                             | sigs == nine  -> '9'
                             | otherwise     -> error "unknown signal"
                             where sigs = fromList sigString
        )
        where signals = map fromList signalStrings
              one = uniqueSize 2 signals
              four = uniqueSize 4 signals
              seven = uniqueSize 3 signals
              eight = uniqueSize 7 signals
              sixes = fromList $ filter (\e -> (size e) == 6) signals
              nine = only $ filter (isSubsetOf four) $ elems sixes
              zero = only $ filter (isSubsetOf one) $ elems $ sixes \\ (singleton nine)
              six = only $ elems $ difference sixes $ fromList [zero, nine]
              fives = fromList $ filter (\e -> (size e) == 5) $ signals
              three = only $ filter (isSubsetOf seven) $ elems fives
              five = only $ filter (flip isSubsetOf six) $ elems fives
              two = only $ elems $ difference fives $ fromList [three, five]

part2 (Entry signals digits) = read $ map solution digits :: Integer
    where solution = solve signals

main = do
        input <- readFile "sample.txt"
        let lines = nlsplit input
        let entries = map barsplit lines
        let answers = map part2 entries
        putStrLn $ show $ sum answers