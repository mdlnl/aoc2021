import Debug.Trace

-----------
-- Input --

sampleStartingPositions = (4 :: Integer, 8 :: Integer)
realStartingPosotions = (9 :: Integer, 6 :: Integer)

------------
-- Update --

playUntil :: (State -> Bool) -> Die -> State -> (Die, State)
playUntil f die state
    | f state   = --trace ("Winner in " ++ show state)
                  (die, state)
    | otherwise = --trace (show state) $
                  playUntil f (consumeNext3 die) $ playOnce state (next3Total die)

hasWinner target state = (score $ player1 state) >= target || (score $ player2 state) >= target

nextPlayer state = if whoseTurn state == 1 then 2 else 1

playOnce :: State -> Integer -> State
playOnce state totalRoll = State { player1   = play currP1 state totalRoll
                                 , player2   = play currP2 state totalRoll
                                 , whoseTurn = nextPlayer state
                                 }
    where currP1 = player1 state
          currP2 = player2 state

newPosition p r
    | np > 10   = np - 10
    | otherwise = np
    where np = p + (r `mod` 10)

-- Only updates the player if it's their turn!
play :: Player -> State -> Integer -> Player
play player state totalRoll
    | number player == whoseTurn state = Player { number = number player
                                                , position = newPos
                                                , score    = score player + newPos
                                                }
    | otherwise                        = player
    where newPos = newPosition (position player) totalRoll

----------
-- Dice --

data Die = Die { upcomingRolls :: [Integer]
               , numRolls      :: Integer }
instance Show Die where
    show die = (show $ next3) ++ "->" ++ (show $ sum next3) ++ " coming; " ++ 
               (show $ numRolls die) ++ " rolls so far"
        where next3 = take 3 $ upcomingRolls die

initialDie = Die { upcomingRolls = map (\x -> 1 + (x-1) `mod` 100) [1..]
                 , numRolls      = 0
                 }

next3Total die = sum $ take 3 $ upcomingRolls die

consumeNext3 die = Die { upcomingRolls = drop 3 $ upcomingRolls die
                       , numRolls = 3 + numRolls die }

----------------
-- Game state --

data Player = Player { number::Integer, position::Integer, score::Integer }
instance Show Player where
    show player = (show $ number player) ++ "=" ++ (show $ score player) ++ "@" ++ (show $ position player)

data State = State { player1       :: Player
                   , player2       :: Player
                   , whoseTurn     :: Integer
                   }
instance Show State where
    show state = (show $ player1 state) ++ "; " ++
                 (show $ player2 state) ++ "; " ++
                 (show $ whoseTurn state) ++ " to play; "

initialState startPos = State { player1       = Player { number=1, position=fst startPos, score=0 }
                              , player2       = Player { number=2, position=snd startPos, score=0 }
                              , whoseTurn     = 1
                              }

---------
-- DFS --

-- precomputed as [ (s, length $ filter (==s) rolls) | s <- [0..10] ]
--  where rolls = [ a + b + c | a <- [1..3], b <- [1..3], c <- [1..3] ]
waysToRoll :: Integer -> Integer
waysToRoll n = [0,0,0,1,3,6,7,6,3,1,0] !! (fromInteger n)

wins target player = (score $ player) >= target

countWins :: Integer -> (Integer, Integer) -> State -> (Integer, Integer)
countWins target w@(p1w, p2w) state
    | hasWinner target state = (p1w + if wins target $ player1 state then 1 else 0
                               ,p2w + if wins target $ player2 state then 1 else 0)
    | otherwise          = foldl (\(p1s, p2s) (p1c, p2c) -> (p1s + p1c, p2s + p2c)) (0,0) children
    where currP1 = player1 state
          currP2 = player2 state
          children = [ ( ways * p1c, ways * p2c )
                     | totalRoll <- [3..9] :: [Integer]
                     , ways <- [waysToRoll totalRoll]
                     , (p1c, p2c) <- [countWins target w
                                  $ State { player1 = play currP1 state totalRoll
                                          , player2 = play currP2 state totalRoll
                                          , whoseTurn = nextPlayer state } ]
                     ]

part1 :: (Integer, Integer) -> Integer
part1 startPos = losingScore * (numRolls finalDie)
    where (finalDie, finalState) = playUntil (hasWinner 1000) initialDie $ initialState startPos
          p1Score = score $ player1 finalState
          p2Score = score $ player2 finalState
          losingScore = minimum [p1Score, p2Score]
