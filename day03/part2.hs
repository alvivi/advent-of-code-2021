import Data.Set (Set)
import qualified Data.Set as Set

type Index = [(Set [Int], Set [Int])]

parse :: String -> [Int]
parse = map (read . pure)

splits :: [Int] -> Index
splits n = snd $ foldr f (n, []) n
  where f 0 (o, acc) = (o, ((Set.singleton o, Set.empty) : acc))
        f 1 (o, acc) = (o, ((Set.empty, Set.singleton o) : acc))

union :: Index -> Index -> Index
union [] [] = []
union ((xz, xo) : xs) ((yz, yo) : ys) = (Set.union xz yz, Set.union xo yo) : union xs ys

toDecimal :: [Int] -> Int
toDecimal = foldl (\x -> (+) (2 * x)) 0

rating :: (Int -> Int -> Bool) -> Index -> Int
rating cmp i = toDecimal $ walk (all i) i
  where
    all ((zs, os) : _) = Set.union zs os
    walk acc [] = head $ Set.elems acc
    walk acc ((zs, os) : tail) =
      let
        zs' = Set.intersection acc zs
        os' = Set.intersection acc os
      in
        if Set.size acc <= 1
        then head $ Set.elems acc
        else
          if cmp (Set.size os') (Set.size zs')
          then walk os' tail
          else walk zs' tail

main :: IO ()
main = do
  input <- map parse <$> lines <$> getContents
  let index = foldr1 union $ map splits $ input
  putStrLn $ show $ (rating (>=) index) * (rating (<) index)
