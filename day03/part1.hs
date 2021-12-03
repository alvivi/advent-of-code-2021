import Data.List

count :: String -> (Int, Int)
count =
  let
    f (z, o) '0' = (z + 1, o)
    f (z, o) '1' = (z, o + 1)
  in
    foldl f (0, 0)

toDecimal :: [Int] -> Int
toDecimal = foldl (\x -> (+) (2 * x)) 0

toUpper :: (Int, Int) -> Int
toUpper (z, o) = if o > z then 1 else 0

toLower :: (Int, Int) -> Int
toLower (z, o) = if o > z then 0 else 1

main :: IO ()
main = do
  contents <- getContents
  let counts = map count $ transpose $ lines contents
  let upper = toDecimal $ map toUpper counts
  let lower = toDecimal $ map toLower counts
  putStrLn $ show $ upper * lower
