--import Data.MultiMap (MultiMap)
--import qualified Data.MultiMap as MM
import Testing

data Amphipod = A | B | C | D deriving (Eq, Show)

data RoomSpot = Top | Bottom deriving (Eq, Show)

data Location = Hallway Int
              | Room Amphipod RoomSpot deriving (Eq, Show)

--type State = MultiMap Amphipod Location

data Move = Move { amphipod::Amphipod, from::Location, to::Location } deriving (Eq, Show)

-- Example:
-- #############
-- #...........#
-- ###B#C#B#D###
  -- #A#D#C#A#
  -- #########
--  0123456789

threshhold A = 2
threshhold B = 4
threshhold C = 6
threshhold D = 8

sdist Top = 1
sdist Bottom = 2

distance (Hallway h) (Hallway j) = abs $ h - j
distance (Hallway h) (Room r s) = (abs $ h - (threshhold r)) + (sdist s)
distance (Room r s) (Room u v) = dt + (sdist s) + (sdist v)
    where tr = threshhold r
          tu = threshhold u
          dt = abs $ tr - tu
distance r h = distance h r

cost (Move a f t) = (costPerStep a) * (distance f t)
    where costPerStep A = 1
          costPerStep B = 10
          costPerStep C = 100
          costPerStep D = 1000

costTest = doTests action [
        TC (Move B (Room C Top) (Hallway 3)) 40,
        TC (Move C (Room B Top) (Room C Top)) 400,
        TC (Move D (Room B Bottom) (Hallway 5)) 3000,
        TC (Move B (Hallway 3) (Room B Bottom)) 30,
        TC (Move B (Room A Top) (Room B Top)) 40,
        TC (Move A (Hallway 0) (Hallway 0)) 0
    ]
    where action (TC i e) = expect ("cost " ++ show i) e $ cost i