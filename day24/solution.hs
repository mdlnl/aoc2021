import Data.Char
import Data.List (intercalate)
import Data.Map (Map, fromList, insert)
import qualified Data.Map as Map
import Data.Maybe
import Split

data Variable = W | X | Y | Z deriving (Eq, Ord, Read, Show)

data Arg = Var Variable | Lit Integer | Empty deriving (Eq, Read, Show)

data Op = Inp | Add | Mul | Div | Mod | Eql deriving (Eq, Read, Show)

data Instruction = Inst { operator::Op, left::Variable, right::Arg } deriving Eq
instance Show Instruction where
    show (Inst Inp var _)    = "inp " ++ shuncap var
    show (Inst op a (Lit x)) = shuncap op ++ " " ++ shuncap a ++ " " ++ show x
    show (Inst op a (Var v)) = shuncap op ++ " " ++ shuncap a ++ " " ++ shuncap v

uncap (c:cs) = toLower c : cs

shuncap :: Show a => a -> String
shuncap = uncap . show

op :: Op -> (Integer -> Integer -> Integer)
op Add = (+)
op Mul = (*)
op Div = div
op Mod = mod
op Eql = \a b -> if a == b then 1 else 0

type Memory = Map Variable Integer

initialMemory = fromList [(W, 0), (X, 0), (Y, 0), (Z, 0)]

get :: Arg -> Memory -> Integer
get (Var v) m = fromJust $ Map.lookup v m
get (Lit x) _ = x

type State = (Memory, [Integer])

step :: State -> Instruction -> State
step (mem, inp:inps) (Inst Inp var _) = (insert var inp mem,      inps)
step (mem, inputs)   instruction      = (insert dest (o a b) mem, inputs) 
    where dest = left instruction
          a = fromJust $ Map.lookup dest mem
          b = get (right instruction) mem
          o = op $ operator instruction

run :: [Integer] -> [Instruction] -> Memory
run inputs = fst . foldl step (initialMemory, inputs)

isValid :: [Instruction] -> Integer -> Bool
isValid program number
    | elem 0 digits = False
    | otherwise     = z == 0
    where digits = map read $ map (:[]) $ show number
          mem = run digits program
          z = fromJust $ Map.lookup Z mem

maxInput = 99999999999999

findMaxValidInput program = filter (isValid program) [99999999999999,99999999999998..] !! 0

-------------
-- Parsing --

capitalize (c:cs) = toUpper c : cs

parseInstruction :: String -> Instruction
parseInstruction line
    | o == Inp                 = Inst Inp a Empty
    | elem (bWord !! 0) "wxyz" = Inst o a $ Var $ (read $ capitalize bWord :: Variable)
    | otherwise                = Inst o a $ Lit $ (read bWord :: Integer)
    where words = split " " line
          opWord : argWords = words
          o = read $ capitalize opWord :: Op
          aWord : _ = argWords
          a = read $ capitalize aWord :: Variable
          [_, bWord] = argWords

parseProgram :: String -> [Instruction]
parseProgram = map parseInstruction . nlsplit

--------------
-- Chunking --

-- Each chunk is an input into the given variable, followed by a bunch of instructions 
-- that have no inputs.
data Chunk = Chunk (Maybe Variable) [Instruction] deriving Show

isInput (Inst Inp _ _) = True
isInput _              = False

chunk :: [Instruction] -> ([Chunk], [Instruction])
chunk [] = ([], [])
chunk (instruction : instructions) = (Chunk maybeInput chunkInstructions : remainingChunks, rem)
    where (chunkInstructions, oneRem) = chunkOnce instructions
          (remainingChunks, rem) = chunk oneRem
          maybeInput = if isInput instruction then Just $ left instruction else Nothing

chunkOnce :: [Instruction] -> ([Instruction], [Instruction])
chunkOnce [] = ([], [])
chunkOnce instructions@(Inst Inp _ _ : _) = ([], instructions)
chunkOnce (instruction : instructions)  = (instruction : chunkInstructions, rem)
    where (chunkInstructions, rem) = chunkOnce instructions

unchunk :: [Chunk] -> [Instruction]
unchunk = concat . map unchunkOnce

unchunkOnce :: Chunk -> [Instruction]
unchunkOnce (Chunk (Just var) instructions) = Inst Inp var Empty : instructions
unchunkOnce (Chunk Nothing instructions)    =                      instructions


-----------
-- Parts --

part1 filename = do
    input <- readFile filename
    let program = parseProgram input
    let (chunks, _) = chunk program
    putStrLn $ show $ unchunk chunks == program
    putStrLn $ show program
    putStrLn $ show chunks
    putStrLn $ show $ unchunk chunks
