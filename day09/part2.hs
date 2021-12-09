import Data.List (sort)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

parse :: String -> Map (Int, Int) Int
parse = foldl step Map.empty . zip [0..] . map (zip [0..]) . lines
  where
    step map (y, line) = foldl (step' y) map line
    step' y map (x, char) = Map.insert (x, y) (read [char]) map

size :: Map (Int, Int) Int -> (Int, Int)
size canvas =
  let keys = Map.keys canvas
  in (maximum $ map fst keys, maximum $ map snd keys)

getBasin :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, Int)
getBasin canvas p = getBasin' Set.empty canvas p
  where
    getBasin' visited canvas p@(x, y) =
      if Set.member p visited
      then Set.empty
      else case Map.lookup p canvas of
        Nothing -> Set.empty
        Just n | n == 9 -> Set.empty
        Just n ->
          let
            tops = getBasin' (Set.union visited $ Set.fromList [p, (x + 1, y), (x, y + 1), (x - 1, y)]) canvas (x, y - 1)
            rights = getBasin' (foldl Set.union visited [tops, Set.fromList [p, (x, y - 1), (x, y + 1), (x - 1, y)]]) canvas (x + 1, y)
            bottoms = getBasin' (foldl Set.union visited [rights, Set.fromList [p, (x, y - 1), (x + 1, y), (x - 1, y)]]) canvas (x, y + 1)
            lefts = getBasin' (foldl Set.union visited [bottoms, Set.fromList [p, (x, y - 1), (x + 1, y), (x, y + 1)]]) canvas (x - 1, y)
          in
            foldl Set.union (Set.singleton p) [tops, rights, bottoms, lefts]

allBasins :: Map (Int, Int) Int -> [Set (Int, Int)]
allBasins canvas = fst $ foldl step ([], Set.empty) coords
  where
    coords = [(x, y) | x <- [0..w], y <- [0..h]]
    (w, h) = size canvas
    step acc@(list, visited) coord =
      if Set.member coord visited
      then acc
      else
        let basin = getBasin canvas coord
        in (basin : list, Set.union visited basin)

main :: IO ()
main = do
  canvas <- parse <$> getContents
  let result = product $ take 3 $ reverse $ sort $ map Set.size $ allBasins canvas
  putStrLn $ show $ result
