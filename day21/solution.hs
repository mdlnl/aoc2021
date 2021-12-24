import Debug.Trace

-----------
-- Input --

sampleStartingPositions = (4 :: Int, 8 :: Int)
realStartingPosotions = (9 :: Int, 6 :: Int)

------------
-- Update --

playUntil :: (State -> Bool) -> State -> State
playUntil f state
    | f state   = --trace ("Winner in " ++ show state)
                  state 
    | otherwise = --trace (show state) $
                  playUntil f $ playOnce state

hasWinner state = (score $ player1 state) >= 1000 || (score $ player2 state) >= 1000

playOnce :: State -> State
playOnce state = State { player1       = newP1
                       , player2       = newP2
                       , whoseTurn     = if whoseTurn state == 1 then 2 else 1
                       , upcomingRolls = drop 3 $ upcomingRolls state
                       , numRolls      = numRolls state + 3
                       }
    where rolls = take 3 $ upcomingRolls state
          currP1 = player1 state
          currP2 = player2 state
          newP1 = if whoseTurn state == 1 then play currP1 state rolls else currP1
          newP2 = if whoseTurn state == 2 then play currP2 state rolls else currP2

newPosition p r
    | np > 10   = np - 10
    | otherwise = np
    where np = p + (r `mod` 10)

play :: Player -> State -> [Int] -> Player
play player state rolls = Player { number = number player
                                 , position = newPos
                                 , score    = score player + newPos
                                 }
    where newPos = --trace (show state) $
                   newPosition (position player) (sum rolls)

----------------
-- Game state --

data Player = Player { number::Int, position::Int, score::Int }
instance Show Player where
    show player = (show $ number player) ++ "=" ++ (show $ score player) ++ "@" ++ (show $ position player)

data State = State { player1       :: Player
                   , player2       :: Player
                   , whoseTurn     :: Int
                   , upcomingRolls :: [Int]
                   , numRolls      :: Int
                   }
instance Show State where
    show state = (show $ player1 state) ++ "; " ++
                 (show $ player2 state) ++ "; " ++
                 (show $ whoseTurn state) ++ " to play; " ++
                 (show $ next3) ++ "->" ++ (show $ sum next3) ++ " coming; " ++ 
                 (show $ numRolls state) ++ " rolls so far"
        where next3 = take 3 $ upcomingRolls state

initialState startPos = State { player1       = Player { number=1, position=fst startPos, score=0 }
                              , player2       = Player { number=2, position=snd startPos, score=0 }
                              , whoseTurn     = 1
                              , upcomingRolls = map (\x -> 1 + (x-1) `mod` 100) [1..]
                              , numRolls      = 0 }

part1 :: (Int, Int) -> Int
part1 startPos = losingScore * (numRolls finalState)
    where finalState = playUntil hasWinner $ initialState startPos
          p1Score = score $ player1 finalState
          p2Score = score $ player2 finalState
          losingScore = minimum [p1Score, p2Score]
