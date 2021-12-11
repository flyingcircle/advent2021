main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (concat . lines) f
  let fish = (frequency . map read . wordsWhen (==',')) inputs
  let pond = toList (fromListWith (+) (fish ++ emptyPond))
  print ("Num of fish: " ++ show (length (simulateDays 256 pond)))