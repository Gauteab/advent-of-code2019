-- import Data.List.Split
import Data.List.Extra hiding (union)
import Data.Either
import Data.Maybe
-- import Data.List
import Data.Set (member, toList, union, fromList, empty, Set, intersection)

data Direction = V | H
    deriving Show

parse :: String -> [[(Char, Int)]]
parse = fmap (fmap f . splitOn ",") . lines
    where f (x:xs) = (x, read xs)
          -- g ('R', i) = (H, i)
          -- g ('L', i) = (H, -i)
          -- g ('U', i) = (V, i)
          -- g ('D', i) = (V, -i)
          
 
move (x, y) (direction, steps) = 
    case direction of
      'R' -> ((x+steps, y), [(x',y) | x' <- [x+1..x+steps]])
      'L' -> ((x-steps, y), [(x',y) | x' <- [x-steps..x-1]])
      'U' -> ((x, y+steps), [(x,y') | y' <- [y+1..y+steps]])
      'D' -> ((x, y-steps), [(x,y') | y' <- [y-steps..y-1]])

wirePath :: [(Char, Int)] -> [(Int, Int)]
wirePath = snd . foldl f init
    where f (position, visited) x = let (newPosition, newVisited) = move position x in (newPosition ,visited ++ newVisited)
          init = ((0,0), [])

-- wireIntersections :: [[(Int, Int)]] -> [(Int, Int)]
wireIntersections paths = cross $ map fromList paths
    where cross [x,y] = intersection x y

solve1 = minimum . map distance . toList . wireIntersections . map wirePath
    where distance (x,y) = abs x + abs y

-- solve2 :: [(Char, Int)] -> [(Int, Int)]
solve2 xs = 
    let paths = map wirePath xs
        intersections = wireIntersections paths
        p x = member (snd x) intersections
        ps =  map (zip [0..]) paths
        [x,y] = map (filter p) ps
        result = filter (\(_,a) -> any (\(_,b) -> a==b) x) y
     in result

main :: IO ()
main = do
    input <- readFile "input"
    let input' = parse input
    print $  (solve1 input')
    -- print $ solve1 input'
    print $ solve2 input'
