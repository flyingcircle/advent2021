import qualified Data.Char as Char

data CaveRoom = Start | End | BigRoom String | SmallRoom String
  deriving Show

type Path = (CaveRoom, CaveRoom)

parseCaveRoom :: [Char] -> CaveRoom
parseCaveRoom "start" = Start
parseCaveRoom "end" = End
parseCaveRoom s
  | Char.isUpper (head s) = BigRoom s
  | otherwise = SmallRoom s

parsePaths :: [[CaveRoom]] -> [Path]
parsePaths = map (\[x,y] -> (x,y))

isStartPath :: Path -> Bool
isStartPath (Start,_) = True
isStartPath (_,Start) = True
isStartPath _ = False

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = parsePaths ((map (map parseCaveRoom . wordsWhen (=='-')) . lines) f)
  print ("solution: " ++ show inputs)


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'