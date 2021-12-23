module Testing (TestCase (..), expect) where

import Data.Maybe

data TestCase a e = TC a e

expect :: (Eq a, Show a) => String -> a -> a -> Maybe String
expect msg expected actual
    | actual == expected = Nothing
    | otherwise          = Just $ "For " ++ msg
                        ++ ", expected " ++ show expected
                        ++ " but was " ++ show actual

