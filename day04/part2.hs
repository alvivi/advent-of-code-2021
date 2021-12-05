import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Board = (Map (Int, Int) Int, Map Int (Int, Int))

parse :: String -> ([Int], [Board])
parse input = (drawns, boards)
  where
    drawns = map read $ splitBy (== ',') $ head $ head groups
    boards = map toBoard $ tail groups
    groups = splitBy (== "") $ lines input

toBoard :: [String] -> Board
toBoard lines = (posIdx, numIdx)
  where
    numIdx = Map.foldrWithKey (\num pos acc -> Map.insert pos num acc) Map.empty posIdx
    posIdx = fromRows Map.empty $ zip [0..] $ map (zip [0..] . map read . words) $ lines
    fromRows = foldl (\idx (row, columns) -> fromColumns row idx columns)
    fromColumns row = foldl (\idx (column, elem) -> Map.insert (column, row) elem idx)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f = foldr g [[]]
  where
    g x acc | f x = [] : acc
    g x (a : as) = (x : a) : as

type State = (Board, Set Int)

hasWon :: State -> Bool
hasWon ((posIdx, numIdx), marked) = any id $ map (all id . map checkPos) allPos
  where
    checkPos pos = Set.member (fromJust $ Map.lookup pos posIdx) marked
    allPos = concat [rowsPos, colsPos]
    rowsPos = map (\row -> [(row, i) | i <- [0..4]]) [0..4]
    colsPos = map (\col -> [(i, col) | i <- [0..4]]) [0..4]

mark :: Int -> State -> State
mark num (board, marked) = (board, Set.insert num marked)

step :: Int -> [State] -> [State]
step num = foldl step' []
  where
    step' acc state =
      let updatedState = mark num state
      in if hasWon updatedState
        then acc
        else updatedState : acc

play :: [Int] -> [State] -> Int
play (num : nums) states =
  case step num states of
    [] -> computeScore (mark num $ head states)
    updatedStates -> play nums updatedStates
  where
    computeScore ((_, numIdx), marked) =
      sum (Set.difference (Map.keysSet numIdx) marked) * num

main :: IO ()
main = do
  (drawns, boards) <- parse <$> getContents
  let initialStates = map (\board -> (board, Set.empty)) boards
  putStrLn $ show $ play drawns initialStates
