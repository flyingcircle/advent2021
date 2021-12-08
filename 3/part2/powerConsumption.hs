import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let bins = (map (concatMap splitChar) . lines) f
  let oxyRating = calcOxyGenRating bins 0
  print ("oxy rating: " ++ show (convert (reverse oxyRating)))
  let co2Rating = calcCo2Rating bins 0
  print ("co2 rating: " ++ show (convert (reverse co2Rating)))
  print ("final life support rating: " ++ show (convert (reverse oxyRating) * convert (reverse co2Rating)))

splitChar :: Char -> [Int]
splitChar c = [digitToInt c]

calcOxyGenRating :: [[Int]] -> Int -> [Int]
calcOxyGenRating bs i = if length bs == 1 then head bs else calcOxyGenRating (calcOxyGenCol bs i) (i+1)
  
calcOxyGenCol :: [[Int]] -> Int -> [[Int]]
calcOxyGenCol = filterAtBit (>=)

calcCo2Rating :: [[Int]] -> Int -> [Int]
calcCo2Rating bs i = if length bs == 1 then head bs else calcCo2Rating (calcCo2Col bs i) (i+1)
  
calcCo2Col :: [[Int]] -> Int -> [[Int]]
calcCo2Col = filterAtBit (<)

filterAtBit :: (Int -> Int -> Bool) -> [[Int]] -> Int -> [[Int]]
filterAtBit f bs i = 
  let j = sum (transpose bs!!i) in
    let cmp = if f j (length bs - j) then (==1) else (==0) in
      filter (\y -> cmp (y!!i)) bs

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs