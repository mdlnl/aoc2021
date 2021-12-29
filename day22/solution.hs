import Data.List (intercalate)
import Data.Maybe
import Debug.Trace
import Split
import Testing

-----------
-- Types --

data Range = Range { from::Int, to::Int } deriving Eq
instance Show Range where show (Range f t) = show f ++ ".." ++ show t

middle :: Range -> Int
middle (Range f t) = (f + t) `div` 2

rlen (Range f t) = t - f + 1

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
topCorner (Range _ xt, Range _ yt, Range _ zt) = (xt, yt, zt)

volume :: Cuboid -> Int
volume (xr, yr, zr) = (rlen xr) * (rlen yr) * (rlen zr)

cuboid :: RebootStep -> Cuboid
cuboid (RebootStep _ xr yr zr) = (xr, yr, zr)

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

data Octree = Node { bottom    :: Point
                   , top       :: Point
                   , child_xyz :: Octree
                   , child_xyZ :: Octree
                   , child_xYz :: Octree
                   , child_xYZ :: Octree
                   , child_Xyz :: Octree
                   , child_XyZ :: Octree
                   , child_XYz :: Octree
                   , child_XYZ :: Octree
                   }
            | Empty
            deriving (Eq)

isEmpty Empty = True
isEmpty _ = False

showOctree indent Empty = indent ++ "()"
showOctree indent node  = (show $ localCuboid node) ++ "{"
                       ++ showChild "-x -y -z " child_xyz
                       ++ showChild "-x -y +z " child_xyZ
                       ++ showChild "-x +y -z " child_xYz
                       ++ showChild "-x +y +z " child_xYZ
                       ++ showChild "+x -y -z " child_Xyz
                       ++ showChild "+x -y +z " child_XyZ
                       ++ showChild "+x +y -z " child_XYz
                       ++ showChild "+x +y +z " child_XYZ
                       ++ " }"
    where showChild s c = if c node == Empty then "" else "\n" ++ indent ++ s ++ showOctree (' ':' ':indent) (c node)
instance Show Octree where show node = "\n" ++ showOctree "" node ++ "\n"

leaf b t = Node { bottom    = b
                , top       = t
                , child_xyz = Empty
                , child_xyZ = Empty
                , child_xYz = Empty
                , child_xYZ = Empty
                , child_Xyz = Empty
                , child_XyZ = Empty
                , child_XYz = Empty
                , child_XYZ = Empty }

withBottom b (Node _ top blo bli bro bri alo ali aro ari) = Node b top blo bli bro bri alo ali aro ari
withTop t (Node bottom _ blo bli bro bri alo ali aro ari) = Node bottom t blo bli bro bri alo ali aro ari
withCuboid (xr, yr, zr) node = withBottom bottom $ withTop top node
    where bottom = (from xr, from yr, from zr)
          top = (to xr, to yr, to zr)

fromCuboid (xr, yr, zr) = leaf bottom top
    where bottom = (from xr, from yr, from zr)
          top = (to xr, to yr, to zr)

branchList Empty = []
branchList (Node _ _ blo bli bro bri alo ali aro ari) = [blo, bli, bro, bri, alo, ali, aro, ari]

localCuboid node = (Range bx tx, Range by ty, Range bz tz)
    where (bx, by, bz) = bottom node
          (tx, ty, tz) = top node

localVolume Empty = 0
localVolume node = (d bx tx) * (d by ty) * (d bz tz)
    where (bx, by, bz) = bottom node
          (tx, ty, tz) = top node
          d b t = t - b + 1

nonempty :: Cuboid -> Bool
nonempty (Range xf xt, Range yf yt, Range zf zt)
    | xf >= xt   = False
    | yf >= yt   = False
    | zf >= zt   = False
    | otherwise = True

octreeVolume :: Octree -> Int
octreeVolume ot = (localVolume ot) + (sum $ map octreeVolume $ branchList ot)

leaf510 = fromCuboid (Range 5 10, Range 50 100, Range 500 1000)

testOctreeVolume = doTests action [
        TC leaf510 (6 * 51 * 501),
        TC Empty 0 ]
    where action (TC i e) = expect "octree volume" e $ octreeVolume i

-- The "bottom" part you get when you cut a range in two.
botChop :: Int -> Range -> Range
botChop at (Range f t) = Range f (min at t)

-- The "top" part you get when you cut a range in two.
topChop :: Int -> Range -> Range
topChop at (Range f t) = Range (max at f) t

insert :: Cuboid -> Octree -> Octree
insert c Empty
    | nonempty c = leaf (bottomCorner c) (topCorner c)
    | otherwise  = Empty
insert c@(xr, yr, zr) ot
    | nonempty c = Node { bottom = bottom ot
                        , top    = newTop
                        , child_xyz = new_xyz
                        , child_xyZ = new_xyZ
                        , child_xYz = new_xYz
                        , child_xYZ = new_xYZ
                        , child_Xyz = new_Xyz
                        , child_XyZ = new_XyZ
                        , child_XYz = new_XYz
                        , child_XYZ = new_XYZ }
    | otherwise  = ot
    where (bx, by, bz) = bottom ot
          new_xyz = insert (botChop bx xr, botChop by yr, botChop bz zr) (child_xyz ot)
          new_xyZ = insert (botChop bx xr, botChop by yr, topChop bz zr) (child_xyZ ot)
          new_xYz = insert (botChop bx xr, topChop by yr, botChop bz zr) (child_xYz ot)
          new_xYZ = insert (botChop bx xr, topChop by yr, topChop bz zr) (child_xYZ ot)
          new_Xyz = insert (topChop bx xr, botChop by yr, botChop bz zr) (child_Xyz ot)
          new_XyZ = insert (topChop bx xr, botChop by yr, topChop bz zr) (child_XyZ ot)
          new_XYz = insert (topChop bx xr, topChop by yr, botChop bz zr) (child_XYz ot)
          new_XYZ = insert (topChop bx xr, topChop by yr, topChop bz zr) (child_XYZ ot)
          (newAroX, newAroY, newAroZ) = bottom new_XYZ
          (tx, ty, tz) = top ot
          newTop = if new_XYZ /= Empty then (min newAroX tx, min newAroY ty, min newAroZ tz) else top ot

testInsertLeaf510 = doTests action [
        TC (Range 1 2, Range 50 100, Range 500 1000) $
                Node { bottom = (5, 50, 500)
                     , top    = (10, 100, 1000)
                     , child_xyz = Empty
                     , child_xyZ = Empty
                     , child_xYz = Empty
                     , child_xYZ = leaf (1, 50, 500) (2, 100, 1000)
                     , child_Xyz = Empty
                     , child_XyZ = Empty
                     , child_XYz = Empty
                     , child_XYZ = Empty }
        ]
    where action (TC i e) = expect ("insert " ++ show i ++ " leaf510") e $ insert i leaf510

-----------
-- Tests --

testAll = do
    testParseRange
    testParseRebootStep
    testInside
    testOctreeVolume
    testInsertLeaf510

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
