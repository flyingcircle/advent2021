import Data.Set (Set)
import Data.Map

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = (map words . lines) f
  let qs = map getInputs inputs
  let as = map getOutputs inputs
  print ("inputs: " ++ show qs)
  print ("outputs: " ++ show as)
  print ("number of 1,4,7,8: " ++ show (sum (map count1478 as)))

getInputs :: [String] -> [String]
getInputs (x:xs) = if x == "|" then [] else x : getInputs xs
getInputs [] = []

getOutputs :: [String] -> [String]
getOutputs (x:xs) = if x == "|" then xs else getOutputs xs
getOutputs [] = []

get1478 :: [String] -> Map(Int,Set)
get1478 (x:xs) = if length x `elem` [2,3,4,7] then 1 + count1478 xs else count1478 xs
get1478 [] = 