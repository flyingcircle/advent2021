import qualified Data.Map as Map

type Rules = Map.Map String Char

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let (phrase:rules) = lines f
  let ruleMap = ruleStringToMap rules
  let freqs = (Map.elems . frequency) (foldl (\p x -> applyRules ruleMap p) phrase [0..39])
  print ("solution: " ++ show (maximum freqs - minimum freqs))

ruleStringToMap :: [String] -> Rules
ruleStringToMap rs = Map.fromList (map ((\ [x,_,[y]] -> (x,y)) . words) rs)

applyRules :: Rules -> String -> String
applyRules rs (a:b:i) = let c = Map.lookup [a,b] rs in
  case c of 
    Just c -> a:c:applyRules rs (b:i)
    Nothing -> a:applyRules rs (b:i)
applyRules _ [a] = [a]
applyRules _ _ = []

frequency :: (Ord k) => [k] -> Map.Map k Int
frequency xs = Map.fromListWith (+) [(x, 1) | x <- xs]
