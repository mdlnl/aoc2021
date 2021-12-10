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

errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137

-- Error pos stack: there was an error at the given position
-- Incomplete stack: got to the end with no errors
-- Complete: got to the end and the stack was empty
data Result = Error Int String | Incomplete String | Complete deriving (Show)

parse line = parseAux line 0 []
parseAux [] pos [] = Complete
parseAux [] pos stack@(_:_) = Incomplete stack
parseAux (c:cs) pos []
    | isLeft c  = parseAux cs (pos + 1) [c]
    | otherwise = Error pos []
parseAux (c:cs) pos stack@(top:pop)
    | isLeft c            = parseAux cs (pos + 1) (c : top : pop)
    | right c == Just top = parseAux cs (pos + 1) pop
    | otherwise           = trace [c] $ Error pos stack

main = do
    input <- readFile "sample.txt"
    let lines = nlsplit input
    let line0 = lines !! 0
    putStrLn line0
    putStrLn $ show $ parse line0
