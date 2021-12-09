import Data.List
import Data.Set (size, fromList, isSubsetOf, singleton, difference, (\\), elems)
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

only [] = error "empty list"
only [x] = x
only _ = error "expected only one element"

uniqueSize n list = only $ filter (\e -> n == (size e)) list

solve signalStrings = (\sigs -> case () of
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
        )
        where signals = fromList signalStrings
              one = uniqueSize 2 signals
              four = uniqueSize 4 signals
              seven = uniqueSize 3 signals
              eight = uniqueSize 7 signals
              sixes = filter (\e -> (size e) == 6) signals
              nine = only $ filter (isSubsetOf four) sixes
              zero = only $ filter (isSubsetOf one) $ sixes \\ (sigleton four)
              six = only $ difference sixes $ fromList [zero, nine]
              fives = filter (\e -> (size e) == 5) signals
              three = only $ filter (isSubsetOf seven) fives
              five = only $ filter (\e -> e \\ six) fives
              two = only $ difference fives $ fromList [three, five]

main = do
        input <- readFile "sample.txt"
        let lines = nlsplit input
        let entries = map barsplit lines
        putStrLn $ intercalate "\n" $ map show entries
