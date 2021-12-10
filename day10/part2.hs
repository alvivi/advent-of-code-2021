import Control.Monad (foldM)
import Data.List (sort)

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
score (Left _) = 0
score (Right list) = foldl score' 0 list
  where
    score' acc '(' = acc * 5 + 1
    score' acc '[' = acc * 5 + 2
    score' acc '{' = acc * 5 + 3
    score' acc '<' = acc * 5 + 4

choose :: [Int] -> Int
choose list = head $ drop (length list' `div` 2) list'
  where
    list' = filter (/= 0) list

main :: IO ()
main = do
  lines <- lines <$> getContents
  putStrLn $ show $ choose $ sort $ map (score . check) lines
