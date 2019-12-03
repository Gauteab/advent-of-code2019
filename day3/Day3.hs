-- import Data.List.Split
import Data.List.Extra hiding (union)
import Data.Either
import Data.Maybe
-- import Data.List
import Data.Set (toList, union, fromList, empty, Set, intersection)

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
      'R' -> ((x+steps, y), fromList [(x',y) | x' <- [x+1..x+steps]])
      'L' -> ((x-steps, y), fromList [(x',y) | x' <- [x-steps..x-1]])
      'U' -> ((x, y+steps), fromList [(x,y') | y' <- [y+1..y+steps]])
      'D' -> ((x, y-steps), fromList [(x,y') | y' <- [y-steps..y-1]])

-- solve1 :: [[(Char, Int)]] -> [Set (Int, Int)]
solve1 xs = minimum $ map distance $ cross $ fmap (snd . foldl f init) xs
    where f (position, visited) x = let (newPosition, newVisited) = move position x in (newPosition , union newVisited visited)
          init = ((0,0), empty)
          distance (x,y) = abs x + abs y
          cross [x,y] = toList $ intersection x y
solve2 = undefined

main :: IO ()
main = do
    input <- readFile "input"
    let input' = parse input
    print $  (solve1 input')
    -- print $ solve1 input'
    -- print $ solve2 input'
