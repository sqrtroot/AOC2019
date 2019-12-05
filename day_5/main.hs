{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Control.Monad.Loops
import           Data.Vector
import Debug.Trace

data Param = Immediate {unParam :: Int}
           | Memory    {unParam :: Int}
  deriving(Show, Eq)


data OpCode = Add Param Param Param -- Add
            | Mul Param Param Param -- Multiply
            | Jne Param Param -- Jump if not zero
            | Jeq Param Param -- Jump if zero
            | Lt  Param Param Param -- Less than
            | Eq Param Param Param -- Equal
            | Inp Param -- Get input
            | Prt Param -- Print value
            | Con Param -- Constant
            | Fin -- End of program
  deriving(Show, Eq)

data State = Running
           | Finished
  deriving(Show, Eq)

data Machine = Machine{
                currId  :: Int,
                program :: Vector Int,
                input   :: Vector Int,
                output  :: Vector Int,
                state   :: State
                      }
  deriving(Show, Eq)

fetch :: Machine -> (Machine, Int) 
fetch m = (m{currId=currId m + 1}, program m ! currId m)

makeParam :: Int -> Int -> Int -> Param
makeParam i o | 10 ^ i > o = Memory
              | c == 1     = Immediate
              | c == 0     = Memory
              | otherwise  = undefined
  where c = (o `quot` (10^i)) `mod` 10

makeOp3 :: Int -> Machine -> (Param -> Param -> Param -> OpCode) -> (Machine, OpCode)
makeOp3 i m p = do
  let (m1, p1) = fetch m
  let (m2, p2) = fetch m1
  let (m3, p3) = fetch m2
  (m3, p (makeParam 2 i p1) (makeParam 3 i p2) (makeParam 4 i p3))

makeOp2 :: Int -> Machine -> (Param -> Param -> OpCode) -> (Machine, OpCode)
makeOp2 i m p = do
  let (m1, p1) = fetch m
  let (m2, p2) = fetch m1
  (m2, p (makeParam 2 i p1) (makeParam 3 i p2))

makeOp1 :: Int -> Machine -> (Param -> OpCode) -> (Machine, OpCode)
makeOp1 i m p = do
  let (m1, p1) = fetch m
  (m1, p (makeParam 2 i p1))


decode :: (Machine, Int) -> (Machine, OpCode)
decode (m, oc) = case oc `mod` 100 of
    01  -> makeOp3 oc m Add
    02  -> makeOp3 oc m Mul
    03  -> makeOp1 oc m Inp
    04  -> makeOp1 oc m Prt
    05  -> makeOp2 oc m Jeq
    06  -> makeOp2 oc m Jne
    07  -> makeOp3 oc m Lt
    08  -> makeOp3 oc m Eq
    99  ->  (m, Fin)
    _  -> makeOp1 oc m Con

getParam :: Machine -> Param -> Int
getParam _ (Immediate a) = a
getParam m (Memory a) = program m ! a

execute :: (Machine, OpCode) -> (Machine, Maybe (Int, Int))
execute (m, Add a b c) = (m, Just (m `getParam` a + m `getParam` b, unParam c))
execute (m, Mul a b c) = (m, Just (m `getParam` a * m `getParam` b, unParam c))
execute (m, Inp a)     = (m{input=Data.Vector.init (input m)}, Just (Data.Vector.last (input m), unParam a))
execute (m, Prt a)     = (m{output=output m `snoc` (m `getParam` a)}, Nothing)
execute (m, Jeq a b)   = (m{currId = if (m `getParam` a) /= 0 then unParam b else currId m}, Nothing)
execute (m, Jne a b)   = (m{currId = if (m `getParam` a) == 0 then unParam b else currId m}, Nothing)
execute (m, Lt a b c)  = (m, Just (if (m `getParam` a) <  (m `getParam` b) then 1 else 0, unParam c))
execute (m, Eq a b c)  = (m, Just (if (m `getParam` a) == (m `getParam` b) then 1 else 0, unParam c))
execute (m, Con _)     = (m, Nothing)
execute (m, Fin)       = (m{state=Finished}, Nothing)


store :: (Machine, Maybe (Int, Int)) -> Machine
store (m, Nothing) = m
store (m, Just (a, b)) = m{program=program m `update` singleton (b,a)}

test :: [Int]
test = [3,0,4,0,99]
test2 :: [Int]
test2 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
  1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
  999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

run :: Machine -> Machine
run m@Machine{state=Running} = run . store . execute . decode . fetch $ m
run m = m

makeMachine :: [Int] -> [Int] -> Machine
makeMachine prog inp = Machine{currId=0, program=fromList prog, input=fromList inp, output=fromList [], state=Running}


main = do
  print "========================="
  print . output . run . makeMachine inProgram $ [1]
  print "========================="
  print . output . run . makeMachine inProgram $ [5]
  print "========================="

inProgram :: [Int]
inProgram = [3,225,1,225,6,6,1100,1,238,225,104,0,101,20,183,224,101,-63,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1101,48,40,225,1101,15,74,225,2,191,40,224,1001,224,-5624,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,62,60,225,1102,92,15,225,102,59,70,224,101,-885,224,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1,35,188,224,1001,224,-84,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1001,66,5,224,1001,224,-65,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1002,218,74,224,101,-2960,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,49,55,224,1001,224,-104,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1102,43,46,225,1102,7,36,225,1102,76,30,225,1102,24,75,224,101,-1800,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1101,43,40,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,226,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,344,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,359,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,374,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,389,1001,223,1,223,107,677,677,224,1002,223,2,223,1006,224,404,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,434,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,107,226,226,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,479,101,1,223,223,8,677,226,224,1002,223,2,223,1006,224,494,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1108,226,677,224,102,2,223,223,1006,224,554,101,1,223,223,108,226,677,224,102,2,223,223,1005,224,569,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,584,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,599,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,1008,677,677,224,102,2,223,223,1006,224,629,1001,223,1,223,107,226,677,224,102,2,223,223,1006,224,644,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,7,226,226,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226]


