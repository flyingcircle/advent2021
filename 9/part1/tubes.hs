import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (map (map digitToInt) . lines) f
  let lows = (sum . map (+1) . findLowPoints) inputs
  print ("solution: " ++ show lows)


findLowPoints :: [[Int]] -> [Int]
findLowPoints is = [is!!x!!y | x <- [0..99], y <- [0..99], isLowPoint is x y]

isLowPoint :: [[Int]] -> Int -> Int -> Bool
isLowPoint is x y = findLowHoriz (is!!x) y && findLowHoriz (transpose is !! y) x

findLowHoriz :: [Int] -> Int -> Bool
findLowHoriz is x
  | x == (length is - 1) = is!!(x-1) > is!!x
  | x == 0 = is!!(x+1) > is!!x
  | otherwise = is!!(x-1) > is!!x && is!!(x+1) > is!!x