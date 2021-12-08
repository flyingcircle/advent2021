import Data.Char (digitToInt)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let bins = (map (concatMap splitChar) . lines) f
  let gamma = calcGamma bins
  let epsilon = calcEpsilonFromGamma gamma
  let str = "The gamma and epsilon is: " ++ show (convert (reverse gamma),convert (reverse epsilon))
  print str
  print ("The final outcome then is: " ++ show (convert (reverse gamma) * convert (reverse epsilon)))

splitChar :: Char -> [Int]
splitChar c = [digitToInt c]

calcEpsilonFromGamma :: [Int] -> [Int]
calcEpsilonFromGamma = map (1 -)

calcGamma :: [[Int]] -> [Int]
calcGamma = map (\x -> if x >= 500 then 1 else 0) . foldr (zipWith (+)) (replicate 12 0)

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs