import qualified Data.Set as Set

type Coord = (Int,Int)
type Fold = (Char,Int)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = map lineToCoord (lines f)
  let inputs' = foldl (\ cs (a,i) -> if a == 'x' then xFold i cs else yFold i cs) inputs folds
  let coords = Set.fromList inputs'
  printPoints (0,0) coords

printPoints :: Coord -> Set.Set Coord -> IO ()
printPoints (x,y) cs
  | x < 40 = do
    putChar c
    printPoints (x+1,y) cs 
  | y < 7 = do
    putStrLn (c : "")
    printPoints (0,y+1) cs
  | otherwise = putStrLn "done"
  where c = if (x,y) `elem` cs then 'X' else '.'


folds :: [Fold]
folds = [('x',655), ('y',447), ('x',327), ('y',223), ('x',163), 
  ('y',111),('x',81), ('y',55),('x',40), ('y',27), ('y',13), ('y',6)]

fold :: Fold -> [Coord] -> [Coord]
fold ('x',i) cs = xFold i cs
fold ('y',i) cs = yFold i cs
fold _ _ = []

yFold :: Int -> [Coord] -> [Coord]
yFold _ [] = []
yFold i ((x,y):cs) = let c' = if y > i then (x,(2*i)-y) else (x,y) in
  c' : yFold i cs

xFold :: Int -> [Coord] -> [Coord]
xFold _ [] = []
xFold i ((x,y):cs) = let c' = if x > i then ((2*i)-x,y) else (x,y) in
  c' : xFold i cs

lineToCoord :: String -> Coord
lineToCoord s = case map read (wordsWhen (==',') s) of
  [x,y] -> (x,y)
  _ -> (0,0)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'