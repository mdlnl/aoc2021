import Data.Maybe
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

-- Error pos c stack: there was an error (character c) at the given position
-- Incomplete stack: got to the end with no errors
-- Complete: got to the end and the stack was empty
data Result = Error Int Char String | Incomplete String | Complete deriving (Show)

isError (Error _ _ _) = True
isError _ = False

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

main = do
    input <- readFile "sample.txt"
    let lines = nlsplit input
    putStrLn $ "part1 " ++ (show $ part1 lines)
