import Split
import Testing

-----------
-- Types --

data Range = Range { from::Int, to::Int } deriving Eq
instance Show Range where show (Range f t) = show f ++ ".." ++ show t

data Value = On | Off deriving (Eq, Show)

data RebootStep = RebootStep { value::Value, xrange::Range, yrange::Range, zrange::Range } deriving Eq
instance Show RebootStep where show (RebootStep v xr yr zr) = show v ++ " "
                                                                     ++ "x=" ++ show xr
                                                                     ++ ",y=" ++ show yr
                                                                     ++ ",z=" ++ show zr

type Point = (Int, Int, Int)

type Cuboid = (Range, Range, Range)

cuboid :: RebootStep -> Cuboid
cuboid (RebootStep _ xr yr zr) = (xr, yr, zr)

-------------
-- Parsing --

parseRange labeledRange = Range { from = read fromStr :: Int, to = read toStr :: Int }
    where [_, fromDotDotTo] = split "=" labeledRange
          [fromStr, toStr] = split ".." fromDotDotTo

testParseRange = doTests action [ TC "x=1..2"         $ Range 1 2
                                , TC "x=-1234..-7890" $ Range (-1234) (-7890) ]
    where action (TC i e) = expect "parseRange" e $ parseRange i

parseValue "on" = On
parseValue "off" = Off

parseRebootStep line = RebootStep { value = parseValue value
                                  , xrange = parseRange xrange
                                  , yrange = parseRange yrange
                                  , zrange = parseRange zrange }
    where [value, ranges] = split " " line
          [xrange, yrange, zrange] = split "," ranges

testParseRebootStep = doTests action [
        TC "on x=1..2,y=-100..500,z=-3..-4"
           $ RebootStep On (Range 1 2) (Range (-100) 500) (Range (-3) (-4)),
        TC "off x=1..2,y=-100..500,z=-3..-4"
           $ RebootStep Off (Range 1 2) (Range (-100) 500) (Range (-3) (-4))
    ]
    where action (TC i e) = expect "parseRebootStep" e $ parseRebootStep i

parseInput input = map parseRebootStep $ nlsplit input

parseFile filename = do
    input <- readFile filename
    return $ parseInput input

----------------
-- Insideness --

inRange :: Int -> Range -> Bool
inRange x (Range from to) = from <= x && x <= to

inside :: Point -> RebootStep -> Bool
inside (x, y, z) (RebootStep _ xr yr zr) =  inRange x xr
                                         && inRange y yr
                                         && inRange z zr

testInside = doTests action [ TC ( -1,  20, 40) False
                            , TC (  0,  20, 40) True
                            , TC ( 11,  20, 40) False
                            ]
    where rs = RebootStep On (Range 0 10) (Range 20 30) (Range 40 50)
          action (TC i e) = expect "inside" e $ inside i rs


stateAfter rebootSteps xyz 
    | values == [] = Off
    | otherwise    = last values
    where values = map value $ filter (inside xyz) rebootSteps

firstInside reverseSteps xyz
    | first == [] = Off
    | otherwise   = value $ first !! 0
    where first = take 1 $ filter (inside xyz) reverseSteps


smallestEnclosingRange :: Range -> Range -> Range
smallestEnclosingRange (Range f1 t1) (Range f2 t2) = Range (min f1 f2) (max t1 t2)

smallestEnclosingCuboid :: Cuboid -> Cuboid -> Cuboid
smallestEnclosingCuboid (cx, cy, cz) (dx, dy, dz) = (xr, yr, zr)
    where xr = smallestEnclosingRange cx dx
          yr = smallestEnclosingRange cy dy
          zr = smallestEnclosingRange cz dz

-----------
-- Tests --

testAll = do
    testParseRange
    testParseRebootStep
    testInside

-------------------
-- Problem parts --

cuboid50 = [ (x, y, z) | x <- [-50..50]
                       , y <- [-50..50]
                       , z <- [-50..50] ]

part1 filename = do
    rebootSteps <- parseFile filename
    let reverseSteps = reverse rebootSteps
    let numberOn = length $ filter (==On) $ map (stateAfter rebootSteps) cuboid50
    --let numberOn = length $ filter (==On) $ map (firstInside reverseSteps) cuboid50
    putStrLn $ show numberOn

part2 filename = do
    rebootSteps <- parseFile filename
    let c0 = cuboid $ rebootSteps !! 0
    let cs = drop 1 $ map cuboid $ rebootSteps
    let sec = foldl smallestEnclosingCuboid c0 cs
    putStrLn $ show sec
