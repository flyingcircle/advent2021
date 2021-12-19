import Data.List (sort)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = lines f
  let missing = filter (/= "") (map (parse []) inputs)
  let scores = sort (map (`scoreErrors` 0) missing)
  let median = scores !! (length scores `div` 2)
  print ("solution: " ++ show median)

parse :: [Char] -> String -> [Char]
parse s "" = s
parse (t:s) (c:cs)
  | isOpen c = parse (c:t:s) cs
  | isMatch t c = parse s cs
  | otherwise = []
parse [] (c:cs)
  | isOpen c = parse [c] cs
  | otherwise = []

scoreErrors :: [Char] -> Int -> Int
scoreErrors [] i = i
scoreErrors ('(':s) i = scoreErrors s ((i*5)+1)
scoreErrors ('[':s) i = scoreErrors s ((i*5)+2)
scoreErrors ('{':s) i = scoreErrors s ((i*5)+3)
scoreErrors ('<':s) i = scoreErrors s ((i*5)+4)
scoreErrors _ _ = 0

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