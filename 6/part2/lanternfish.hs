import Data.Map (fromList, fromListWith, toList, Map, updateAt)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (concat . lines) f
  let fish = (map read . wordsWhen (==',')) inputs :: [Int]
  let initFish = addFish fish emptyPond
  print ("Fish: " ++ show (sum (foldl (\x _ -> simDay x) initFish [0..255])))

emptyPond :: [Int]
emptyPond = replicate 10 0

addFish :: [Int] -> [Int] -> [Int]
addFish fs = zipWith (+) (map (`count` fs) [0..9])

simDay :: [Int] -> [Int]
simDay = decrementFish . birthFish

birthFish :: [Int] -> [Int]
birthFish fs = zipWith (+) [if x == 7 || x == 9 then head fs else 0 | x <- [0..9]] fs

decrementFish :: [Int] -> [Int]
decrementFish (x:y:fs) = y : decrementFish (y:fs)
decrementFish [y] = [0]
decrementFish [] = []

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'