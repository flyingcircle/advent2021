
main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = lines f
  print ("solution: " ++ show (scoreErrors (concatMap (parse []) inputs)))

parse :: [Char] -> String -> [Char]
parse s "" = []
parse (t:s) (c:cs)
  | isOpen c = parse (c:t:s) cs
  | isMatch t c = parse s cs
  | otherwise = [c]
parse [] (c:cs)
  | isOpen c = parse [c] cs
  | otherwise = [c]

scoreErrors :: [Char] -> Int
scoreErrors s = sum (zipWith (*) (map (countLetters s) ")]}>") [3,57,1197,25137])

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

isOpen :: Char -> Bool
isOpen c = c `elem` "([{<"

isClose :: Char -> Bool
isClose c = c `elem` ")]}>"

isMatch :: Char -> Char -> Bool
isMatch '(' ')' = True
isMatch '[' ']' = True
isMatch '{' '}' = True
isMatch '<' '>' = True
isMatch _ _ = False