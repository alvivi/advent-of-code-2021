import Data.Map (Map)
import qualified Data.Map as Map

type Pos = (Int, Int)
type Line = (Pos, Pos)

parse :: String -> [Line]
parse = map toPairs . map words . lines
  where
    toPairs (start : _ : end : []) = (toPos start, toPos end)
    toPos str =
      case splitBy (== ',') str of
        (x : y : []) -> (read x, read y)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = foldr g [[]]
  where
    g x acc | f x = [] : acc
    g x (a : as) = (x : a) : as

lineToPos :: Line -> [Pos]
lineToPos ((x1, y1), (x2, y2)) = zip' (range x1 x2) (range y1 y2)
  where
    zip' (x : []) (y : []) = [(x, y)]
    zip' xs@(x : []) (y : ys) = (x, y) : zip' xs ys
    zip' (x : xs) ys@(y : []) = (x, y) : zip' xs ys
    zip' (x : xs) (y : ys) = (x, y) : zip' xs ys
    range a b | a <= b = [a..b]
    range a b = reverse [b..a]

draw :: [Line] -> Map Pos Int
draw  = foldl (\acc pos -> Map.alter inc pos acc) Map.empty . concat . map lineToPos
  where
    inc Nothing = Just 1
    inc (Just v) = Just $ v + 1

count :: Map Pos Int -> Int
count = sum . map (fromEnum . (>= 2)) . Map.elems

main :: IO ()
main = do
  lines <- parse <$> getContents
  putStrLn $ show $ count $ draw lines
