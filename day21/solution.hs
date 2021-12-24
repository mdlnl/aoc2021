-----------
-- Input --

sampleStartingPositions = (4, 8)
realStartingPosotions = (9, 6)

------------
-- Update --

playUntilWin :: State -> State
playUntilWin state
    | (score $ player1 state) >= 1000 = state
    | (score $ player2 state) >= 1000 = state
    | otherwise                       = playUntilWin $ playOnce state

playOnce :: State -> State
playOnce state = State { player1       = newP1
                       , player2       = newP2
                       , whoseTurn     = if whoseTurn state == 1 then 2 else 1
                       , upcomingRolls = drop 3 $ upcomingRolls state
                       , numRolls      = numRolls state + 1
                       }
    where rolls = take 3 $ upcomingRolls state
          currP1 = player1 state
          currP2 = player2 state
          newP1 = if whoseTurn state == 1 then play currP1 state rolls else currP1
          newP2 = if whoseTurn state == 2 then play currP2 state rolls else currP2

newPosition initialPosition totalRoll = finalZeroBased + 1
    where initialZeroBased = initialPosition - 1
          finalZeroBased = (initialZeroBased + 1) `mod` 10

play player state rolls = Player { number = number player
                                 , position = newPos
                                 , score    = score player + newPos
                                 }
    where newPos = newPosition (position player) (sum rolls)

----------------
-- Game state --

data Player = Player { number::Int, position::Int, score::Int } deriving Show

data State = State { player1       :: Player
                   , player2       :: Player
                   , whoseTurn     :: Int
                   , upcomingRolls :: [Int]
                   , numRolls      :: Int
                   } deriving Show

initialState startPos = State { player1       = Player { number=1, position=fst startPos, score=0 }
                              , player2       = Player { number=2, position=snd startPos, score=0 }
                              , whoseTurn     = 1
                              , upcomingRolls = [1..]
                              , numRolls      = 0 }

part1 :: (Int, Int) -> Int
part1 startPos = losingScore * (numRolls finalState)
    where finalState = playUntilWin $ initialState startPos
          p1Score = score $ player1 finalState
          p2Score = score $ player2 finalState
          losingScore = min (p1Score, p2Score)
