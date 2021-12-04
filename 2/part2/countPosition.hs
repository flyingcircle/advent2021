
data Direction = FORWARD Int | UP Int | DOWN Int

main = do
  f <- readFile "../inputs.txt"
  let inputStrings = lines f
  let dirs = map words inputStrings
  let (horizontal, depth, aim) = foldl countPosition (0,0,0) (map inputToDirection dirs)
  let str = "The number of increases is: " ++ show (horizontal,depth)
  print str
  print ("The final outcome then is: " ++ show (horizontal * depth))

inputToDirection :: [String] -> Direction
inputToDirection ["forward", y] = FORWARD (read y)
inputToDirection ["up", y] = UP (read y)
inputToDirection ["down", y] = DOWN (read y)
inputToDirection _ = FORWARD 0

countPosition :: (Int,Int,Int) -> Direction ->  (Int, Int, Int)
countPosition (h,d,a) (FORWARD x)  = (h + x, d + (a * x), a)
countPosition (h,d,a) (UP x) = (h, d, a - x)
countPosition (h,d,a) (DOWN x) = (h, d, a + x)
