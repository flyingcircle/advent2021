import Data.Char (digitToInt, intToDigit)
import Data.List (transpose)

main :: IO ()
main = do
  f <- readFile "../test.txt"
  let inputs = (map (map digitToInt) . lines) f
  print ("solution: " ++ show (map (map intToDigit) (transformToBasinMap inputs)))

transformToBasinMap :: [[Int]] -> [[Int]]
transformToBasinMap = map (map (\x -> if x == 9 then 0 else 1))

findLowPoints :: [[Int]] -> [Int]
findLowPoints is = [is!!x!!y | x <- [0..99], y <- [0..99], isLowPoint is x y]

isLowPoint :: [[Int]] -> Int -> Int -> Bool
isLowPoint is x y = findLowHoriz (is!!x) y && findLowHoriz (transpose is !! y) x

findLowHoriz :: [Int] -> Int -> Bool
findLowHoriz is x
  | x == (length is - 1) = is!!(x-1) > is!!x
  | x == 0 = is!!(x+1) > is!!x
  | otherwise = is!!(x-1) > is!!x && is!!(x+1) > is!!x