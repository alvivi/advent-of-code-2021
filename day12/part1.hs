import Data.Char (isUpper)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

parse :: String -> Map String [String]
parse = makeSymmetric . foldl insert Map.empty . map (break (== '-')) . lines
  where
    insert acc (from, ('-' : to)) = Map.alter (update to) from acc
    update to Nothing = Just [to]
    update to (Just list) = Just (nub (to : list))
    makeSymmetric graph = Map.foldlWithKey fill graph graph
    fill acc from dests = foldl (\acc dest -> Map.alter (update from) dest acc) acc dests

getAllPaths :: Map String [String] -> [[String]]
getAllPaths graph = fromJust $ step ["start"] (Set.singleton "start")
  where
    step acc@("end" : _) _ = Just [acc]
    step acc@(last : _) visited =
      case Map.lookup last graph of
        Nothing -> Nothing
        Just dests ->
          let paths = map (continue acc visited) dests
          in case concat $ map fromJust $ filter isJust paths of
            [] -> Nothing
            paths' -> Just paths'
    continue _ visited dest | not (all isUpper dest) && Set.member dest visited = Nothing
    continue acc visited dest = step (dest : acc) (Set.insert dest visited)

main :: IO ()
main = do
  graph <- parse <$> getContents
  putStrLn $ show $ length $ getAllPaths graph
