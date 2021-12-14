import Data.List (iterate, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

inc :: (Ord k) => Int -> k -> Map k Int -> Map k Int
inc n = Map.alter (Just . maybe n (+ n))

parse :: String -> (Map String Char, Map String Int)
parse input = (pairs, histogram)
  where
    histogram = foldl (flip $ inc 1) Map.empty pairs'
    pairs' = map (\(x, y) -> x : y : []) $ zip template (tail template)
    ((template : []), ("" : pairLines)) = break (== "") $ lines $ input
    pairs = foldl insert Map.empty pairLines
    insert pairs line =
      let (key : ("->" : (value : []))) = words line
      in Map.insert key (head value) pairs

step :: Map String Char -> Map String Int -> Map String Int
step pairs histogram =
  Map.foldlWithKey (\acc p@(x : (y : [])) count ->
   case Map.lookup p pairs of
     Nothing -> Map.insert p count acc
     Just z -> inc count (z : (y : [])) $ inc count (x : (z : [])) acc
  ) Map.empty histogram

stepMany :: Int -> Map String Char -> Map String Int -> Map String Int
stepMany n pairs histogram = iterate (step pairs) histogram !! n

solve :: Map String Char -> Map String Int -> Int
solve pairs histogram = last countList - head countList
  where
    countList = sort $ Map.elems counts
    counts =
      Map.foldlWithKey (\acc (_ : (y : [])) count -> inc count y acc) Map.empty
        $ iterate (step pairs) histogram !! 40

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ show $ uncurry solve input
