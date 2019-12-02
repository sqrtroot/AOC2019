{-# LANGUAGE NamedFieldPuns #-}
module Main where
import           Control.Monad.Loops
import           Data.Vector

data OpCode = Add Int Int Int
            | Mul Int Int Int
            | Con Int
            | Fin
  deriving(Show, Eq)

data Machine = Machine{
                currId  :: Int,
                program :: Vector Int
                      }
  deriving(Show, Eq)

makeOpCode :: Vector Int -> Int -> OpCode
makeOpCode p i | p ! i == 1  = Add (p ! (i+1)) (p ! (i+2)) (p ! (i+3))
               | p ! i == 2  = Mul (p ! (i+1)) (p ! (i+2)) (p ! (i+3))
               | p ! i == 99 = Fin
               | otherwise   = Con (p ! i)

machineArrithmetic :: Int -> Int -> Int -> (Int -> Int -> Int) -> Machine -> Machine
machineArrithmetic o1 o2 o3 f m@Machine{currId , program} =
  m{currId = currId+4,
    program=update program (singleton (o3, f (program ! o1) (program ! o2)))}

step :: Machine -> Maybe Machine
step m@Machine{currId, program}= case makeOpCode program $ currId of
                                   Add o1 o2 o3 -> Just (machineArrithmetic o1 o2 o3 (+) m)
                                   Mul o1 o2 o3 -> Just (machineArrithmetic o1 o2 o3 (*) m)
                                   Con _        -> Just m{currId=currId + 1}
                                   Fin          -> Nothing

makeMachine :: [Int] -> Machine
makeMachine i = Machine{currId=0, program=fromList i}

run :: Machine -> Machine
run m = case step m of
          Just x  -> run x
          Nothing -> m

fixProgram :: Int -> Int -> Machine -> Machine
fixProgram a b m@Machine{program} = m{program=update program (fromList [(1,a),(2,b)])}

machineOutput :: Int -> Int -> Machine -> Int
machineOutput a b = Data.Vector.head . program . run . fixProgram a b

solution1 :: Machine -> Int
solution1 = machineOutput 12 2

input :: Machine
input = makeMachine [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,10,19,23,1,23,9,27,1,5,27,31,2,31,13,35,1,35,5,39,1,39,5,43,2,13,43,47,2,47,10,51,1,51,6,55,2,55,9,59,1,59,5,63,1,63,13,67,2,67,6,71,1,71,5,75,1,75,5,79,1,79,9,83,1,10,83,87,1,87,10,91,1,91,9,95,1,10,95,99,1,10,99,103,2,103,10,107,1,107,9,111,2,6,111,115,1,5,115,119,2,119,13,123,1,6,123,127,2,9,127,131,1,131,5,135,1,135,13,139,1,139,10,143,1,2,143,147,1,147,10,0,99,2,0,14,0]

solution2 :: Machine -> Maybe (Int,Int)
solution2 = solution2' 0 0
solution2' :: Int -> Int -> Machine -> Maybe (Int, Int)
solution2' a b c | machineOutput a b c == 19690720 = Just (a, b)
  | otherwise = if a > 99 then Nothing else (if b > 99 then solution2' (a+1) 0 c else solution2' a (b+1) c)

main = do
  print (solution1 input)
  print (solution2 input)
