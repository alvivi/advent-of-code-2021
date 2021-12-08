import Data.List (permutations, sort)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map

render :: String -> Maybe Char
render signals = Map.lookup (sort signals) shapes
  where
    shapes = Map.fromList
      [ ("abcefg", '0')
      , ("cf", '1')
      , ("acdeg", '2')
      , ("acdfg", '3')
      , ("bcdf", '4')
      , ("abdfg", '5')
      , ("abdefg", '6')
      , ("acf", '7')
      , ("abcdefg", '8')
      , ("abcdfg", '9')
      ]

mappings :: [Map Char Char]
mappings = map (Map.fromList . zip "abcdefg") $ permutations "abcdefg"

solve :: String -> Int
solve line = readOutput $ choose $ map tryRender mappings
 where
   applyMapping mapping = map (\c -> fromJust $ Map.lookup c mapping)
   choose = map fromJust . head . filter (all isJust)
   inputs = filter (/= "|") $ words line
   readOutput = read . reverse . take 4 . reverse
   tryRender mapping = map render $ map (applyMapping mapping) inputs

main :: IO ()
main = do
  lines <- lines <$> getContents
  putStrLn $ show $ sum $ map solve lines
