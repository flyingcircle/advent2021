import Data.Map (fromList, fromListWith, toList, Map, updateAt)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (concat . lines) f
  let fish = (frequency . map read . wordsWhen (==',')) inputs
  let pond = toList (fromListWith (+) (fish ++ emptyPond))
  print ("Num of fish: " ++ show (length (simulateDays 256 pond)))

emptyPond :: Map Integer Integer
emptyPond = fromList [(x, 0) | x <- [0..8]]

simulateDays :: Int -> [(Int,Int)] -> [(Int,Int)]
simulateDays d f = if d == 0 then f else simulateDays (d-1) (simulateDay f)

simulateDay :: [(Int,Int)] -> [(Int,Int)]
simulateDay = map (\x -> x - 1) . replaceZeros . newFish

replaceZeros :: [(Int,Int)] -> [(Int,Int)]

newFish :: (Eq a, Num a) => [a] -> [a]

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

frequency :: (Ord k, Num a) => [k] -> Map k a
frequency xs = fromListWith (+) [(x, 1) | x <- xs]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'