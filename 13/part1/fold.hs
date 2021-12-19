import qualified Data.Set as Set

type Coord = (Int,Int)
type Fold = (Char,Int)

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = map lineToCoord (lines f)
  let inputs' = xFold 655 inputs
  let count = length (Set.fromList inputs')
  print ("solution: " ++ show count)

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