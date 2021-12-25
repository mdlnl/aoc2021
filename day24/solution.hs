import Data.Map (Map, fromList, insert)
import qualified Data.Map as Map
import Data.Maybe
import Split

data Variable = W | X | Y | Z deriving (Eq, Ord)

data Arg = Var Variable | Lit Integer | Empty

data Op = Ins | Add | Mul | Div | Mod | Eql

data Instruction = Inst { operator::Op, left::Variable, right::Arg }

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
step (mem, inp:inps) (Inst Ins var _) = (insert var inp mem,      inps)
step (mem, inputs)   instruction      = (insert dest (o a b) mem, inputs) 
    where dest = left instruction
          a = fromJust $ Map.lookup dest mem
          b = get (right instruction) mem
          o = op $ operator instruction

run :: [Integer] -> [Instruction] -> Memory
run inputs = fst . foldl step (initialMemory, inputs)
