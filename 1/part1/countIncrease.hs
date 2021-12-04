
main = do
  f <- readFile "../inputs.txt"
  let numStrings = lines f
  let nums = map read numStrings
  let str = "The number of increases is: " ++ show (increaseCount nums)
  print str

increaseCount :: [Int] -> Int
increaseCount (x:y:xs) = if y > x then 1 + increaseCount (y:xs) else increaseCount (y:xs)
increaseCount _ = 0