module Main where

calculateFuel :: RealFrac a => a -> a
calculateFuel m = fromIntegral (floor (m / 3.0)) - 2.0

strToFloat :: String -> Float
strToFloat = read

puzzle1 :: [Float] -> Float
puzzle1 = sum . map calculateFuel

puzzle2 :: [Float] -> Float
puzzle2 inp = if null nextFuels then sum fuelReqs else sum fuelReqs + puzzle2 nextFuels
  where fuelReqs = map calculateFuel inp
        nextFuels = filter (\y -> calculateFuel y >= 0) fuelReqs
  
main = do
  x <- map strToFloat . lines <$> readFile "input_1.txt"
  print (puzzle1 x)
  print (puzzle2 x)

