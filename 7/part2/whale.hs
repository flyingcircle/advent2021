

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (concat . lines) f
  let crabPositions = (map read . wordsWhen (==',')) inputs :: [Int]
  print ("The best fuel cost is: " ++ show (calcFuelFromPosition crabPositions (div (sum crabPositions) (length crabPositions))))

calcFuelFromPosition :: [Int] -> Int -> Int
calcFuelFromPosition cs p = (sum . map ((\x -> ((x*x)+x) `div` 2) . abs . (\x -> x - p))) cs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'