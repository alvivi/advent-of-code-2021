import Data.Map (Map)
import qualified Data.Map as Map

parse :: String -> Map (Int, Int) Int
parse = foldl step Map.empty . zip [0..] . map (zip [0..]) . lines
  where
    step map (y, line) = foldl (step' y) map line
    step' y map (x, char) = Map.insert (x, y) (read [char]) map

getSize :: Map (Int, Int) Int -> (Int, Int)
getSize canvas =
  let keys = Map.keys canvas
  in (maximum $ map fst keys, maximum $ map snd keys)

step :: Map (Int, Int) Int -> Map (Int, Int) Int
step canvas = Map.map reset $ foldl inc canvas coords
  where
    size = getSize canvas
    coords = [(x, y) | y <- [0..9],  x <- [0..9]]
    inc canvas coord =
      let canvas' = Map.adjust (+ 1) coord canvas
      in case Map.lookup coord canvas of
          Just 9 -> foldl inc canvas' $ getNeighbors coord
          Just e -> canvas'
          Nothing -> canvas
    reset n = if n > 9 then 0 else n
    getNeighbors (x, y) =
      [ (x - 1, y - 1)
      , (x, y - 1)
      , (x + 1, y - 1)
      , (x - 1, y)
      , (x + 1, y)
      , (x - 1, y + 1)
      , (x, y + 1)
      , (x + 1, y + 1)
      ]

run :: Map (Int, Int) Int -> Int -> Int
run canvas steps = run' 0 canvas steps
  where
    run' acc _ 0 = acc
    run' acc canvas steps =
      let
        canvas' = step canvas
        acc' = acc + count canvas'
      in
        run' acc' canvas' (steps - 1)
    count = length . filter (== 0) . Map.elems

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ show $ run input 100

