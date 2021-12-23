module Testing (TestCase (..), expect) where

import Data.Maybe

data TestCase a e = TC a e

expect msg e a
    | a == e    = Nothing
    | otherwise = Just $ "For " ++ msg ++ ", expected " ++ e ++ " but was " ++ a
