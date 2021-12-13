import Data.List (intersperse)
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

foldPaper :: Set (Int, Int) -> [(Int, Int)] -> Set (Int, Int)
foldPaper paper fs = foldl (\acc f -> Set.map (flipCood f) acc) paper fs
  where
    flipCood (fx, fy) (x, y) = (flipValue fx x, flipValue fy y)
    flipValue 0 value = value
    flipValue flip value | value > flip = flip - (value - flip)
    flipValue _ value = value

getSize :: Set (Int, Int) -> (Int, Int)
getSize paper =
  let keys = Set.elems paper
  in (maximum $ map fst keys, maximum $ map snd keys)

render :: Set (Int, Int) -> String
render paper = map paint coords
  where
    paint (_, y) | y > h = '\n'
    paint coord = if Set.member coord paper then '#' else '.'
    coords = [(x, y) | x <- [0..w], y <- [0..(h + 1)]]
    (w, h) = getSize paper

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ render $ uncurry foldPaper input

