main =
  let f list = sum $ map fromEnum $ map (uncurry (<)) $ zip list (tail list)
  in do
    contents <- getContents
    putStrLn $ show $ f $ map (read :: String -> Int) $ lines $ contents
