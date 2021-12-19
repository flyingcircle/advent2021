

main :: IO ()
main = do
  f <- readFile "../inputs.txt"
  let inputs = lines f
  print ("Num of fish: " ++ show (length (simulateDays 256 pond)))