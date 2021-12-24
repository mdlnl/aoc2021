import Debug.Trace

-----------
-- Input --

sampleStartingPositions = (4 :: Integer, 8 :: Integer)
realStartingPosotions = (9 :: Integer, 6 :: Integer)

------------
-- Update --

playUntil :: (State -> Bool) -> State -> State
playUntil f state
    | f state   = --trace ("Winner in " ++ show state)
                  state 
    | otherwise = --trace (show state) $
                  playUntil f $ playOnce state

hasWinner target state = (score $ player1 state) >= target || (score $ player2 state) >= target

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

play :: Player -> State -> [Integer] -> Player
play player state rolls = Player { number = number player
                                 , position = newPos
                                 , score    = score player + newPos
                                 }
    where newPos = --trace (show state) $
                   newPosition (position player) (sum rolls)

----------------
-- Game state --

data Player = Player { number::Integer, position::Integer, score::Integer }
instance Show Player where
    show player = (show $ number player) ++ "=" ++ (show $ score player) ++ "@" ++ (show $ position player)

data State = State { player1       :: Player
                   , player2       :: Player
                   , whoseTurn     :: Integer
                   , upcomingRolls :: [Integer]
                   , numRolls      :: Integer
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

part1 :: (Integer, Integer) -> Integer
part1 startPos = losingScore * (numRolls finalState)
    where finalState = playUntil (hasWinner 1000) $ initialState startPos
          p1Score = score $ player1 finalState
          p2Score = score $ player2 finalState
          losingScore = minimum [p1Score, p2Score]
