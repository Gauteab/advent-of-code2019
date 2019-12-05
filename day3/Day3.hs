import Data.List.Extra hiding (union)
import qualified Data.Map.Strict as Map
import Data.Set (member, toList, union, fromList, empty, Set, intersection)

parse :: String -> [String]
parse = fmap (concatMap f . splitOn ",") . lines
    where f (x:xs) = replicate (read xs) x
          
 
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) direction = 
    case direction of
      'R' -> (x+1, y)
      'L' -> (x-1, y)
      'U' -> (x, y+1)
      'D' -> (x, y-1)

wirePath :: String -> [(Int, Int)]
wirePath = snd . foldl f init
    where init = ((0,0), []) 
          f (position, visited) direction = 
            let newPosition = move position direction 
             in (newPosition , newPosition : visited)
          

solve1 :: [String] -> Int
solve1 = minimum . map distance . toList . wireIntersections . map wirePath
    where distance (x,y) = abs x + abs y
          wireIntersections paths = cross $ map fromList paths
          cross [x,y] = intersection x y

solve2 :: [String] -> Int
solve2 paths = 
    let [p1, p2] = map (Map.fromList . flip zip [1..] . reverse . wirePath) paths
     in minimum $ map snd $ Map.toList $ Map.intersectionWith (+) p1 p2

main :: IO ()
main = do
    input <- readFile "input"
    let input' = parse input
    print $ solve1 input' -- 386
    print $ solve2 input' -- 6484
