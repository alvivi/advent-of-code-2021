move (x, y) ["forward", dx] = (x + read dx, y)
move (x, y) ["down", dy] = (x, y + read dy)
move (x, y) ["up", dy] = (x, y - read dy)

main = do
  contents <- getContents
  putStrLn $ show $ uncurry (*) $ foldl move (0, 0) $ map words $ lines contents
