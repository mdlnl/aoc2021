import Data.Maybe
--import Data.Sort
import Debug.Trace
import Split
import System.IO

left '(' = Just ')'
left '[' = Just ']'
left '{' = Just '}'
left '<' = Just '>'
left _ = Nothing
isLeft = isJust . left

right ')' = Just '('
right ']' = Just '['
right '}' = Just '{'
right '>' = Just '<'
right _ = Nothing
isRight = isJust . right

data Result =
   Error Int Char String | -- there was an error (character c) at the given position
   Incomplete String     | -- got to the end with no errors
   Complete                -- got to the end and the stack was empty
   deriving (Show)

isError (Error _ _ _) = True
isError _ = False

isIncomplete (Incomplete _) = True
isIncomplete _ = False

parse line = parseAux line 0 []
parseAux [] pos [] = Complete
parseAux [] pos stack@(_:_) = Incomplete stack
parseAux (c:cs) pos []
    | isLeft c  = parseAux cs (pos + 1) [c]
    | otherwise = Error pos c []
parseAux (c:cs) pos stack@(top:pop)
    | isLeft c            = parseAux cs (pos + 1) (c : top : pop)
    | right c == Just top = parseAux cs (pos + 1) pop
    | otherwise           = Error pos c stack

errorScore (Error _ ')' _) = 3
errorScore (Error _ ']' _) = 57
errorScore (Error _ '}' _) = 1197
errorScore (Error _ '>' _) = 25137
errorScore _ = 0

part1 lines = sum [errorScore $ parse line | line <- lines]

autocompleteScore '(' = 1
autocompleteScore '[' = 2
autocompleteScore '{' = 3
autocompleteScore '<' = 4
autocompleteScore c = error $ "unexpected stack item " ++ [c]

totalAutocompleteScore (Incomplete stack) = foldl acc 0 stack
    where acc score top = score * 5 + (autocompleteScore top)

quicksort [] = []
quicksort (x:xs) = left ++ (x : right)
    where left = quicksort $ filter (<x) xs
          right = quicksort $ filter (>=x) xs

part2 lines = scores !! m
    where results = map parse lines
          scores = quicksort $ map totalAutocompleteScore $ filter isIncomplete results
          m = (length scores) `div` 2

main = do
    input <- readFile "full.txt"
    let lines = nlsplit input
    putStrLn $ "part1 " ++ (show $ part1 lines)
    putStrLn $ "part2 " ++ (show $ part2 lines)
