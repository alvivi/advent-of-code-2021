import Data.List (iterate, sort)
import Data.Map (Map)
import qualified Data.Map as Map

parse :: String -> (Map String Char, String)
parse input = (pairs, template)
  where
    ((template : []), ("" : pairLines)) = break (== "") $ lines $ input
    pairs = foldl insert Map.empty pairLines
    insert pairs line =
      let (key : ("->" : (value : []))) = words line
      in Map.insert key (head value) pairs

step :: Map String Char -> String -> String
step pairs (x : (y : xs)) =
  case Map.lookup (x : (y : [])) pairs of
    Nothing -> x : (step pairs (y : xs))
    Just r -> x : (r : (step pairs (y : xs)))
step pairs input = input

stepMany :: Int -> Map String Char -> String -> String
stepMany n pairs template = iterate (step pairs) template !! n

computeHistogram :: String -> Map Char Int
computeHistogram = foldl (flip $ Map.alter inc) Map.empty
  where
    inc Nothing = Just 1
    inc (Just count) = Just $ count + 1

solve :: Map String Char -> String -> Int
solve pairs template = last counts - head counts
  where
    counts = sort $ Map.elems $ computeHistogram $ iterate (step pairs) template !! 10

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ show $ uncurry solve input
