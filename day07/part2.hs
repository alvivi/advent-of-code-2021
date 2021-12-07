splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = foldr g [[]]
  where
    g x acc | f x = [] : acc
    g x (a : as) = (x : a) : as

compute :: [Int] -> Int
compute nums = snd $ minPair
  where
    max = maximum nums
    min = minimum nums
    diffs = map (\n -> sum $ map (\x -> fuel (abs (n - x))) nums) [min..max]
    fuel limit = sum [0..limit]
    pairs = zip [min..max] diffs
    minPair = foldl stepMinPair (maxBound, maxBound) pairs
    stepMinPair a@(ai, av) (xi, xv) | av <= xv = a
    stepMinPair (ai, av) x@(xi, xv) = x

main :: IO ()
main = do
  nums <- map (read :: String -> Int) <$> splitBy (== ',') <$> getContents
  putStrLn $ show $ compute nums
