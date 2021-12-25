module Testing (TestCase (..), doTests, expect) where

import Data.List (intercalate)
import Data.Maybe

data TestCase i e = TC i e

expect :: (Eq a, Show a) => String -> a -> a -> Maybe String
expect msg expected actual
    | actual == expected = Nothing
    | otherwise          = Just $ "For " ++ msg
                        ++ ", expected " ++ show expected
                        ++ " but was " ++ show actual

doTests :: (Eq a, Show a) => (TestCase i a -> Maybe String) -> [TestCase i a] -> IO ()
doTests action cases
    | errors == [] = do return ()
    | otherwise    = do putStrLn $ intercalate "\n" errors
    where errors = map fromJust $ filter isJust $ map action cases
