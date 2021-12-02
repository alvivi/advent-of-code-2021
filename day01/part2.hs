import Data.List

main =
  let
    g list = map sum $ filter ((== 3) . length) $ transpose [list, (tail list), (tail $ tail list)]
    f list = sum $ map fromEnum $ map (uncurry (<)) $ zip list (tail list)
  in do
    contents <- getContents
    putStrLn $ show $ f $ g $ map (read :: String -> Int) $ lines $ contents
