import Data.List (transpose)

data Cell = Marked Int | Unmarked Int
instance Show Cell where
  show (Marked i) = show i
  show (Unmarked i) = show i

type Board = [[Cell]]

type BingoBalls = [Int]

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let (bingoInputs:boardInputs) = lines f
  let bingoBalls = map read (wordsWhen (==',') bingoInputs) :: [Int]
  let boards = parseBoards boardInputs
  let (winningBoard,winningNum) = playGame boards bingoBalls
  let str = "The final placement is: " ++ show (winningBoard,winningNum)
  print str
  print ("The winning number then is: " ++ show (calcScore winningBoard winningNum))

calcScore :: Board -> Int -> Int
calcScore b i = let f = concat b in
  (sum . map getUnmarkedValue) f * i

getUnmarkedValue :: Cell -> Int
getUnmarkedValue (Unmarked i) = i
getUnmarkedValue (Marked i) = 0

playGame :: [Board] -> BingoBalls -> (Board,Int)
playGame bs (i:is) = 
  let newBoards = markBoards i bs in
    case anyWinningBoard newBoards of
      Just b -> (b,i)
      Nothing -> playGame newBoards is
playGame _ _ = ([],0)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


parseBoards :: [String] -> [Board]
parseBoards (_:a:b:c:d:e:xs) = newBoard (map (map read . words) [a,b,c,d,e]) : parseBoards xs
parseBoards _ = []

newBoard :: [[Int]] -> Board
newBoard = map (map Unmarked)

markBoards :: Int -> [Board] -> [Board]
markBoards i = map (markBoard i)

markBoard :: Int -> Board -> Board
markBoard i = map (map (markCell i))

anyWinningBoard :: [Board] -> Maybe Board
anyWinningBoard bs = case b of
  w : _ -> Just w
  _ -> Nothing
  where b = filter isWinningBoard bs

isWinningBoard :: Board -> Bool
isWinningBoard b = any (all isMarked) b || any (all isMarked) (transpose b)

isMarked :: Cell -> Bool
isMarked (Marked _) = True
isMarked (Unmarked _) = False

markCell :: Int -> Cell -> Cell
markCell i (Unmarked c)= if c == i then Marked c else Unmarked c
markCell _ c = c