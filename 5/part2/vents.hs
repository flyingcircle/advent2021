import Data.Map (fromListWith, toList)

type Coords = (Int, Int, Int, Int)

type Coord = (Int, Int)

type Board = [[Int]]

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = lines f
  let vents = (parseCoords . map words) inputs
  let placedVents = frequency (concatMap calcCoords vents)
  print ("Num of overlaps: " ++ show (countOverlap placedVents))

parseCoords :: [[String]] -> [Coords]
parseCoords = map parseCoord

parseCoord :: [String] -> Coords
parseCoord [c1,_,c2] = 
  let [x1, y1] = wordsWhen (==',') c1 in
    let [x2, y2] = wordsWhen (==',') c2 in
      (read x1, read y1, read x2, read y2)
parseCoord _ = (0,0,0,0)

calcCoords :: Coords -> [Coord]
calcCoords (x1,y1,x2,y2)
  | x1 == x2 = [(x1,y3) | y3 <- ys]
  | y1 == y2 = [(x3,y1) | x3 <- xs]
  | y1 > y2 && x1 < x2 = zip xs (reverse ys)
  | x1 > x2 && y1 < y2 = zip (reverse xs) ys
  | otherwise = zip xs ys
  where ys = if y2 > y1 then [y1..y2] else [y2..y1]
        xs = if x2 > x1 then [x1..x2] else [x2..x1]

doesCoordOverlap :: (Coord,Int) -> Bool
doesCoordOverlap (_,i) = i > 1

countOverlap :: [(Coord,Int)] -> Int
countOverlap = length . filter doesCoordOverlap

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'