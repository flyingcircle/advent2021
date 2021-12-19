import Data.Char (digitToInt, intToDigit)
import Data.List (transpose)
import Data.Set (Set,fromList,member,elemAt)

main :: IO ()
main = do
  f <- readFile "../test.txt"
  let inputs = (map (map digitToInt) . lines) f
  print ("coords: " ++ show(transformToCoords inputs))

transformToCoords :: [[Int]] -> Set (Int,Int)
transformToCoords is = fromList [(x,y) | x <- [0..4], y <- [0..9], ((is!!x)!!y) < 9]

getAdjCoords :: (Num a1, Num a2) => (a1, a2) -> [(a1, a2)]
getAdjCoords (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

getBasins :: Set (Int,Int) -> [Set (Int,Int)]
getBasins cs =  b : getBasins 
  where 
    c = elemAt 0 cs
    b = getBasin c cs


getBasin :: (Ord a1, Ord a2, Num a1, Num a2) => (a1, a2) -> Set (a1, a2) -> Set (a1, a2)
getBasin b c = fromList (filter (`member` c) (getAdjCoords b))