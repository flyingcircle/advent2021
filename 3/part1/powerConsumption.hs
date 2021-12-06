{-# LANGUAGE BinaryLiterals #-}
import Data.Bits (Bits(complement, (.&.)))
import Data.Char (digitToInt)

main = do
  f <- readFile "../inputs.txt"
  let bins = lines f
  let gamma = calcGamma bins
  let epsilon = calcEpsilonFromGamma gamma
  let str = "The gamma and epsilon is: " ++ show (gamma,epsilon)
  print str
  print ("The final outcome then is: " ++ show (gamma * epsilon))

calcEpsilonFromGamma :: Int -> Int
calcEpsilonFromGamma = (.&.) 4095 . complement

calcGamma :: [String] -> Int
calcGamma xs = let bitSums = sumPositions xs in
  convert (map (\x -> if x >= (length xs `div` 2) then 1 else 0) bitSums)

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs

binStrsToInts :: [String] -> [[Int]]
binStrsToInts = map (map digitToInt)

sumPositions :: [String] -> [Int]
sumPositions = foldl1 (zipWith (+)) . binStrsToInts