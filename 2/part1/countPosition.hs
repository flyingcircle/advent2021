
data Direction = FORWARD Int | UP Int | DOWN Int

main = do
  f <- readFile "../inputs.txt"
  let inputStrings = lines f
  let dirs = map words inputStrings
  let (horizontal, depth) = foldr (countPosition . inputToDirection) (0,0) dirs
  let str = "The final placement is: " ++ show (horizontal,depth)
  print str
  print ("The final outcome then is: " ++ show (horizontal * depth))

inputToDirection :: [String] -> Direction
inputToDirection ["forward", y] = FORWARD (read y)
inputToDirection ["up", y] = UP (read y)
inputToDirection ["down", y] = DOWN (read y)
inputToDirection _ = FORWARD 0

countPosition :: Direction -> (Int,Int) -> (Int, Int)
countPosition (FORWARD x) (h,d) = (h + x, d)
countPosition (UP x) (h,d) = (h, d - x)
countPosition (DOWN x) (h,d) = (h, d + x)
