import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace
import Split (nlsplit)

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
parseSnailfish depth s@(c:cs) = PR (R ival) rem
    where PR ival rem = parseInt s

parseInt (c:',':cs) = PR (read [c]) (',':cs)
parseInt (c:']':cs) = PR (read [c]) (']':cs)
parseInt (c:d:',':cs) = PR (read [c,d]) (',':cs)
parseInt (c:d:']':cs) = PR (read [c,d]) (']':cs)

parsePair depth ('[':s) = PR (P depth p1 p2) p2rem
    where PR p1 (',':p1rem) = parseSnailfish (depth + 1) s
          PR p2 (']':p2rem) = parseSnailfish (depth + 1) p1rem

---------------
-- Explosion --

data ExplosionState = Searching | Found Int | Done deriving (Eq, Show)
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

explode sf0 = explodeLeft Searching sf1
    where ExplosionStep after0 sf1 = explodeRight Searching sf0 -- has to happen first since we want leftmost explodable pair

{-
explosionTests = do
    putStrLn $ show $ t "[[[[[9,8],1],2],3],4]"                 "[[[[0,9],2],3],4]"
    putStrLn $ show $ t "[7,[6,[5,[4,[3,2]]]]]"                 "[7,[6,[5,[7,0]]]]"
    putStrLn $ show $ t "[[6,[5,[4,[3,2]]]],1]"                 "[[6,[5,[7,0]]],3]"
    putStrLn $ show $ t "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
    putStrLn $ show $ t "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"     "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
    where t = test explode "explode"
    -}

-----------
-- Split --

data SplitState = Splitting | Split deriving (Eq, Show)
data SplitStep = SplitStep SplitState Snailfish deriving Show

crack (SplitStep _ s) = s

split0 = split 0 Splitting

split _ Split s = SplitStep Split s
split d Splitting (R r)
    | r >= 10   = SplitStep Split     (P d (R $ floor $ r%2) (R $ ceiling $ r%2))
    | otherwise = SplitStep Splitting (R r)
split d state (P _ a b) = SplitStep afterB (P d ar br)
    where SplitStep afterA ar = split (d + 1) state a
          SplitStep afterB br = split (d + 1) afterA b

{-
splitTests = do
    putStrLn $ show $ t "[[[[0,7],4],[15,[0,13]]],[1,1]]"    "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
    putStrLn $ show $ t "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
    where t = test (crack . split0) "split"
    -}

------------
-- Reduce --

didExplode (ExplosionStep Done _) = True
didExplode (ExplosionStep (Found _) _) = True
didExplode _ = False

reduce s
    | didExplode e    = --trace ("exploded " ++ pretty s) $
                        reduce eResult
    | sFinal == Split = --trace ("split    " ++ pretty s) $
                        reduce sResult
    | otherwise       = s
    where e@(ExplosionStep _ eResult) = explode s
          SplitStep sFinal sResult = split0 s

deepen (R r) = R r
deepen (P d a b) = P (d+1) (deepen a) (deepen b)

add s t = P 0 (deepen s) (deepen t)

-------------
-- Testing --

data TestResult = Pass String | Fail String deriving Show

test :: (Snailfish -> Snailfish) -> String -> String -> String -> TestResult
test f desc input expected
    | x == e    = Pass $ desc ++ " " ++ input
    | otherwise = Fail $ desc ++ " " ++ input
    where x = f $ parse input
          e = parse $ expected

------------
-- Part 1 --

addAll (s:ss) = foldl (\a b -> reduce $ add a b) s ss

magnitude (R r) = r
magnitude (P _ a b) = 3 * (magnitude a) + 2 * (magnitude b)

part1 filename = do
    input <- readFile filename
    putStrLn $ show $ magnitude $ addAll $ map parse $ nlsplit input

------------
-- Part 2 --

part2 filename = do
    input <- readFile filename
    let numbers = map parse $ nlsplit input
    putStrLn $ show $ maximum $ [magnitude $ reduce $ add a b | a <- numbers, b <- numbers, a /= b]
