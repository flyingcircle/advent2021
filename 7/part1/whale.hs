

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (concat . lines) f
  let crabPositions = (map read . wordsWhen (==',')) inputs :: [Int]
  print ("The best fuel cost is: " ++ show (findBestPosition crabPositions 0))

findBestPosition :: [Int] -> Int -> Int
findBestPosition cs p = 
  let next = calcFuelFromPosition cs (p+1) in
    let curr = calcFuelFromPosition cs p in
      if curr < next then curr else findBestPosition cs (p+1)


calcFuelFromPosition :: [Int] -> Int -> Int
calcFuelFromPosition cs p = (sum . map (abs . (\x -> x - p))) cs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'