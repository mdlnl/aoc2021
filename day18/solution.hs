import Data.List
import Data.Maybe

data Snailfish = R Int
               | P Int Snailfish Snailfish
               | X Int -- tracks where an explosion happened, as well as the leftgoing blast
               deriving (Eq, Show)
--instance Show Snailfish where

pretty (R r) = show r
pretty (P _ p1 p2) = "[" ++ pretty p1 ++ "," ++ pretty p2 ++ "]"
pretty (X a) = show a ++ "!"

toParenthesesMap c
    | c == '['  = '('
    | c == ']'  = ')'
    | otherwise = c
toParentheses = map toParenthesesMap

data ParseResult a = PR a String deriving (Show)

result (PR r _) = r

parse string = result
    where PR result [] = parseSnailfish 0 string

parseSnailfish depth s@('[':_) = parsePair depth s
parseSnailfish depth s@(c:cs) = PR (R (read [c])) cs

parsePair depth ('[':s) = PR (P depth p1 p2) p2rem
    where PR p1 (',':p1rem) = parseSnailfish (depth + 1) s
          PR p2 (']':p2rem) = parseSnailfish (depth + 1) p1rem

---------------
-- Explosion --

data ExplosionState = Searching | Found Int | Done deriving Show
data ExplosionStep = ExplosionStep ExplosionState Snailfish deriving Show

boom (ExplosionStep _ s) = s

explodeRight Done s = ExplosionStep Done s
explodeRight (Found n) (R r) = ExplosionStep Done (R $ n + r)
explodeRight state (R r) = ExplosionStep state (R r)
explodeRight Searching (P 4 (R a) (R b)) = ExplosionStep (Found b) (X a)
explodeRight state (P d a b) = ExplosionStep afterB (P d ar br)
    where ExplosionStep afterA ar = explodeRight state a
          ExplosionStep afterB br = explodeRight afterA b

explodeLeft Done s = ExplosionStep Done s
explodeLeft (Found n) (R r) = ExplosionStep Done (R $ n + r)
explodeLeft state (R r) = ExplosionStep state (R r)
explodeLeft Searching (X a) = ExplosionStep (Found a) (R 0)
explodeLeft state (P d a b) = ExplosionStep afterA (P d ar br)
    where ExplosionStep afterB br = explodeLeft state b
          ExplosionStep afterA ar = explodeLeft afterB a

explode sf0 = sf2
    where ExplosionStep after0 sf1 = explodeRight Searching sf0 -- has to happen first since we want leftmost explodable pair
          ExplosionStep after1 sf2 = explodeLeft  Searching sf1

-----------
-- Split --



-------------
-- Testing --

data TestResult = Pass String | Fail String deriving Show

explosionTest input expected
    | x == e    = Pass $ "explode " ++ input
    | otherwise = Fail $ "explode " ++ input
    where x = explode $ parse input
          e = parse $ expected

explosionTests = do
    putStrLn $ show $ explosionTest "[[[[[9,8],1],2],3],4]"                 "[[[[0,9],2],3],4]"
    putStrLn $ show $ explosionTest "[7,[6,[5,[4,[3,2]]]]]"                 "[7,[6,[5,[7,0]]]]"
    putStrLn $ show $ explosionTest "[[6,[5,[4,[3,2]]]],1]"                 "[[6,[5,[7,0]]],3]"
    putStrLn $ show $ explosionTest "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
    putStrLn $ show $ explosionTest "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"     "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
