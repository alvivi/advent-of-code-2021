splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = foldr g [[]]
  where
    g x acc | f x = [] : acc
    g x (a : as) = (x : a) : as

count 0 _ = 0
count days 0 = 1 + count (days - 1) 6 + count (days - 1) 8
count days state = count (days - 1) (state - 1)

main :: IO ()
main = do
  initialStates <- map read <$> splitBy (== ',') <$> getContents
  let childrenCount = sum $ map (count 80) initialStates
  putStrLn $ show $ (childrenCount + length initialStates)
