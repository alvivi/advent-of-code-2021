import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> (Set (Int, Int), [(Int, Int)])
parse input = (coords, folds)
  where
    coords = foldl (flip Set.insert) Set.empty $ map parseCoord coordLines
    folds = map parseFold foldLines
    (coordLines, ("" : foldLines)) = break (== "") $ lines $ input
    parseCoord line = let (x, (',' : y)) = break (== ',') line in (read x, read y)
    parseFold line =
      let (title, ('=' : value)) = break (== '=') line
      in case last title of
        'x' -> (read value, 0)
        'y' -> (0, read value)

foldPaper :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
foldPaper f = Set.map (flipCood f)
  where
    flipCood (fx, fy) (x, y) = (flipValue fx x, flipValue fy y)
    flipValue 0 value = value
    flipValue flip value | value > flip = flip - (value - flip)
    flipValue _ value = value

main :: IO ()
main = do
  (paper, (fold : _)) <- parse <$> getContents
  putStrLn $ show $ Set.size $ foldPaper fold paper

