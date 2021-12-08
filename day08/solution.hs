import Data.List
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

barsplit line = Entry (map sort $ words before) (map sort $ words after)
        where (before, after) = splitAtDelim " | " line

main = do
        input <- readFile "sample.txt"
        let lines = nlsplit input
        let entries = map barsplit lines
        putStrLn $ intercalate "\n" $ map show entries
