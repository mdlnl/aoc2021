import Data.Maybe

data Snailfish = R Int | P Int Snailfish Snailfish deriving Eq
instance Show Snailfish where
    show (R r) = show r
    show (P _ p1 p2) = "[" ++ show p1 ++ "," ++ show p2 ++ "]"

toParenthesesMap c
    | c == '['  = '('
    | c == ']'  = ')'
    | otherwise = c
toParentheses = map toParenthesesMap

data ParseResult a = PR a String deriving (Show)

result (PR r _) = r

parseSnailfish depth s@('[':_) = parsePair depth s
parseSnailfish depth s@(c:cs) = PR (R (read [c])) cs

parsePair depth ('[':s) = PR (P depth p1 p2) p2rem
    where PR p1 (',':p1rem) = parseSnailfish (depth + 1) s
          PR p2 (']':p2rem) = parseSnailfish (depth + 1) p1rem

--data Explosion = Bang Snailfish Int Int | Whimper Snailfish deriving Eq

--maybeExplode s@(R _) = Whimper s
--maybeExplode s@(P 3 (P 4 (R r4a) (R r4b)) right)
--    | hasRegular right = Bang (P 3 (R 0) (addToFirstRegular r4b right)) r4a 0
--    | otherwise        = Bang (P 3 (R 0) right) r4a r4b
--maybeExplode (P d a b)
--    | maybeExplode a == Bang afterA leftBlast rightBlast && hasRegular b = Bang (P d afterA (addToFirstRegular rightBlast b)) leftBlast 0
--    | maybeExplode a == Bang afterA leftBlast rightBlast                 = Bang (P d afterA b) leftBlast rightBlast
--    | maybeExplode b == Bang afterB leftBlast rightBlast && hasRegular a = Bang (P d (addToFirstRegular leftBlast a) afterB) 0 rightBlast
--    | maybeExplode b == Bang afterB leftBlast rightBlast                 = Bang (P d a afterB) leftBlast rightBlast
--    | otherwise                                                          = Whimper s
--
--hasRegular (R _) = True
--hasRegular (P _ a b) = (hasRegular a) || (hasRegular b)
--
--addToFirstRegular n (R r) = R $ r + n
--addToFirstRegular n (P d a b)
--    | hasRegular a = P d (addToFirstRegular n a) b
--    | hasRegular b = P d a (addToFirstRegular n b)
--    | otherwise    = P d a b

