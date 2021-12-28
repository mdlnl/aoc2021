import Data.Maybe
import Split
import Testing

-----------
-- Types --

data Range = Range { from::Int, to::Int } deriving Eq
instance Show Range where show (Range f t) = show f ++ ".." ++ show t

middle :: Range -> Int
middle (R f t) = (f + t) `div` 2

rlen (R f t) = t - f + 1

data Value = On | Off deriving (Eq, Show)

data RebootStep = RebootStep { value::Value, xrange::Range, yrange::Range, zrange::Range } deriving Eq
instance Show RebootStep where show (RebootStep v xr yr zr) = show v ++ " "
                                                                     ++ "x=" ++ show xr
                                                                     ++ ",y=" ++ show yr
                                                                     ++ ",z=" ++ show zr

type Point = (Int, Int, Int)

type Cuboid = (Range, Range, Range)

bottomCorner :: Cuboid -> Point
bottomCorner (Range xf _, Range yf _, Range zf _) = (xf, yf, zf)

topCorner :: Cuboid -> Point
topCorner (Range _ xt, Range yt, Range zt) = (xt, yt, zt)

volume :: Cuboid -> Int
volume (xr, yr, zr) = (rlen xr) * (rlen yr) * (rlen zr)

cuboid :: RebootStep -> Cuboid
cuboid (RebootStep _ xr yr zr) = (xr, yr, zr)

({=) :: Cuboid -> Point -> Bool
(xr yr zr) {{ (x y z) = from xr <= x && x <= to xr
                     && from yr <= y && y <= to yr
                     && from zr <= z && z <= to zr

({{) :: Cuboid -> Point -> Bool
(xr yr zr) {{ (x y z) = from xr <= x && x < to xr
                     && from yr <= y && y < to yr
                     && from zr <= z && z < to zr

center :: Cuboid -> Point
center (xr, yr, zr) = (middle xr, middle yr, middle zr)

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

-----------------------------------
-- Octree for storing cuboids in --

data Octree = Node { bottom        :: Point
                   , top           :: Point
                   , belowLeftOut  :: Octree
                   , belowLeftIn   :: Octree
                   , belowRightOut :: Octree
                   , belowRightIn  :: Octree
                   , aboveLeftOut  :: Octree
                   , aboveLeftIn   :: Octree
                   , aboveRightOut :: Octree
                   , aboveRightIn  :: Octree
                   }
            | Empty

leaf r c = Node { bottom        = r
                , top           = c
                , belowLeftOut  = Empty
                , belowLeftIn   = Empty
                , belowRightOut = Empty
                , belowRightIn  = Empty
                , aboveLeftOut  = Empty
                , aboveLeftIn   = Empty
                , aboveRightOut = Empty
                , aboveRightIn  = Empty }

branchList Empty = []
branchList (Node _ _ blo bli bro bri alo ali aro ari) = [blo, bli, bro, bri, alo, ali, aro, ari]

localVolume Empty = 0
localVolume node = (d bx tx) * (d by ty) * (d bz tz)
    where (bx, by, bz) = bottom node
          (tx, ty, tz) = top node
          d b t = t - b + 1

nonempty :: Cuboid -> Bool
nonempty (Range xf xt, Range yf yt, Range zf zt)
    | xf > xt   = False
    | yf > yt   = False
    | zf > zt   = False
    | otherwise = True

octreeVolume :: Octree -> Int
octreeVolume ot = (localVolume ot) + sum $ map octreeVolume $ branchList ot

testOctreeVolume = doTests action [
        TC Empty 0 ]
    where action (TC i e) = expect "octree volume" e $ octreeVolume i

-- The "bottom" part you get when you cut a range in two
botChop :: Int -> Range -> Range
botChop at (Range f t) = Range f (min at t)

-- The "top" part you get when you cut a range in two
topChop :: Int -> Range -> Range
topChop at (Range f t) = Range (max at f) t

insert :: Cuboid -> Octree -> Octree
insert c Empty
    | nonempty c = leaf (bottomCorner c) (topCorner c)
    | otherwise  = Empty
insert c@(xr, yr, zr) ot
    | nonempty c = Empty -- TODO: implement this
    | otherwise  = ot
    where (rx, ry, rz) = bottom ot
          newBro = insert (topChop rx xr, botChop ry yr, topChop rz zr) (bottomRightOut ot)
          newBri = insert (topChop rx xr, botChop ry yr, botChop rz zr) (bottomRightIn ot)
          newBlo = insert (botChop rx xr, botChop ry yr, topChop rz zr) (bottomRightOut ot)
          newBli = insert (botChop rx xr, botChop ry yr, botChop rz zr) (bottomRightIn ot)
          newTro = insert (topChop rx xr, topChop ry yr, topChop rz zr) (bottomRightOut ot)
          newTri = insert (topChop rx xr, topChop ry yr, botChop rz zr) (bottomRightIn ot)
          newTlo = insert (botChop rx xr, topChop ry yr, topChop rz zr) (bottomRightOut ot)
          newTli = insert (botChop rx xr, topChop ry yr, botChop rz zr) (bottomRightIn ot)

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
    let sec@(xr, yr, zr) = foldl smallestEnclosingCuboid c0 cs
    putStrLn $ show sec
    let secPoints = [ (x, y, z) | x <- [from xr..to xr]
                                , y <- [from yr..to yr]
                                , z <- [from zr..to zr] ]
    let reverseSteps = reverse rebootSteps
    let numberOn = length $ filter (==On) $ map (firstInside reverseSteps) secPoints
    putStrLn $ show numberOn
