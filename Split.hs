module Split ( split, nlsplit ) where

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

nlsplit = split "\n"
