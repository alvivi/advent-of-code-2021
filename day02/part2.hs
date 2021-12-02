move (a, (x, y)) ["forward", dx] = (a, (x + read dx, y + a * read dx))
move (a, (x, y)) ["down", dy] = ((a + read dy), (x, y))
move (a, (x, y)) ["up", dy] = ((a - read dy), (x, y))

main = do
  contents <- getContents
  putStrLn $ show $ uncurry (*) $ snd $ foldl move (0, (0, 0)) $ map words $ lines contents
