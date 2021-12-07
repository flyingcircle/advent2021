
main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (concat . lines) f
  let fish = (map read . wordsWhen (==',')) inputs :: [Int]
  print ("Num of fish: " ++ show (length (simulateDays 80 fish)))

simulateDays :: Int -> [Int] -> [Int]
simulateDays d f = if d == 0 then f else simulateDays (d-1) (simulateDay f)

simulateDay :: [Int] -> [Int]
simulateDay = map (\x -> x - 1) . replaceZeros . newFish

replaceZeros = map (\x -> if x == 0 then 7 else x)

newFish :: (Eq a, Num a) => [a] -> [a]
newFish f = f ++ replicate (count 0 f) 9

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'