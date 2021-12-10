import Control.Monad (foldM)

check :: String -> Either Char [Char]
check = foldM check' []
  where
    check' stack '(' = Right $ '(' : stack
    check' stack '[' = Right $ '[' : stack
    check' stack '{' = Right $ '{' : stack
    check' stack '<' = Right $ '<' : stack
    check' ('(' : stack) ')' = Right stack
    check' ('[' : stack) ']' = Right stack
    check' ('{' : stack) '}' = Right stack
    check' ('<' : stack) '>' = Right stack
    check' (expected : stack) found = Left found

score :: Either Char [Char] -> Int
score (Left ')') = 3
score (Left ']') = 57
score (Left '}') = 1197
score (Left '>') = 25137
score _ = 0

main :: IO ()
main = do
  lines <- lines <$> getContents
  putStrLn $ show $ sum $ map (score . check) lines
