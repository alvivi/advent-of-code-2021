import Data.Map (Map)
import qualified Data.Map as Map

type Cache = Map (Int, Int) Int

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = foldr g [[]]
  where
    g x acc | f x = [] : acc
    g x (a : as) = (x : a) : as

count :: Cache -> Int -> Int -> (Cache, Int)
count cache 0 _ = (cache, 0)
count cache days 0 =
  case Map.lookup (days, 0) cache of
    Just cachedResult -> (cache, cachedResult)
    Nothing ->
      let result' = 1 + siblingCount + childCount
      in (Map.insert (days, 0) result' updatedCache', result')
  where
    (updatedCache, siblingCount) = count cache (days - 1) 6
    (updatedCache', childCount) = count updatedCache (days - 1) 8
count cache days state =
  case Map.lookup (days, state) cache of
    Just cachedResult -> (cache, cachedResult)
    Nothing ->
      let (updatedCache, result) = count cache (days - 1) (state - 1)
      in (Map.insert (days, state) result updatedCache, result)

countMany :: Cache -> Int -> [Int] -> (Cache, Int)
countMany cache days = foldl step (Map.empty, 0)
  where
    step (cache, acc) num =
      let (updatedCache, result) = count cache days num
      in (updatedCache, result + acc)

main :: IO ()
main = do
  initialStates <- map read <$> splitBy (== ',') <$> getContents
  let (_, childrenCount) = countMany Map.empty 256 initialStates
  putStrLn $ show $ (childrenCount + length initialStates)
