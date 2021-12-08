count :: String -> Int
count = length . filter check . parse
  where
    parse = concat . map (map length . words . tail . dropWhile (/= '|')) . lines
    check n | n == 2 || n == 4 || n == 3 || n == 7 = True
    check _ = False

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show $ count contents
