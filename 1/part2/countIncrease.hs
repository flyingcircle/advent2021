
main = do
  f <- readFile "../inputs.txt"
  let numStrings = lines f
  let nums = map read numStrings
  let str = "The number of increases is: " ++ show (increaseCount nums)
  print str

increaseCount :: [Int] -> Int
increaseCount (a:b:c:d:xs) = if d > a then 1 + increaseCount (b:c:d:xs) else increaseCount (b:c:d:xs)
increaseCount _ = 0