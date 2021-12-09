
import Data.Maybe (fromJust, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

parse :: String -> Map (Int, Int) Int
parse = foldl step Map.empty . zip [0..] . map (zip [0..]) . lines
  where
    step map (y, line) = foldl (step' y) map line
    step' y map (x, char) = Map.insert (x, y) (read [char]) map

risk :: Map (Int, Int) Int -> (Int, Int) -> Int
risk canvas p@(x, y) =
    if all (value <) [top, right, bottom, left]
    then value + 1
    else 0
  where
    value = fromJust $ Map.lookup p canvas
    top = fromMaybe 10 $ Map.lookup (x, y - 1) canvas
    bottom = fromMaybe 10 $ Map.lookup (x, y + 1) canvas
    left = fromMaybe 10 $ Map.lookup (x - 1, y) canvas
    right = fromMaybe 10 $ Map.lookup (x + 1, y) canvas

size :: Map (Int, Int) Int -> (Int, Int)
size canvas =
  let keys = Map.keys canvas
  in (maximum $ map fst keys, maximum $ map snd keys)

totalRisk :: Map (Int, Int) Int -> Int
totalRisk canvas = sum $ map (risk canvas) [(x, y) | x <- [0..w], y <- [0..h]]
  where (w, h) = size canvas

main :: IO ()
main = do
  canvas <- parse <$> getContents
  putStrLn $ show $ totalRisk canvas
