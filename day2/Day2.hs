import Data.List.Split
import Data.Either
import Data.Maybe
import Data.List
import Data.Vector hiding (find,head)

parse :: String -> Vector Int
parse = fromList . fmap read . splitOn ","

type State = Either Int (Int, Vector Int)

execute :: (Int -> Int -> Int) -> Int -> Vector Int -> Vector Int
execute op i v = v // [(v!(i+3), op (v!(v!(i+1))) (v!(v!(i+2))))]

step :: State -> State
step (Right (i, v)) = 
    case v!i of
         99 -> Left (v!0)
         1 -> Right (i+4, execute (+) i v)
         2 -> Right (i+4, execute (*) i v)

solve1 :: Int -> Int -> (Vector Int) -> Int
solve1 noun verb v = head $ lefts $ iterate step initState
    where initState = (Right (0, v // [(1,noun), (2,verb)]))

solve2 :: Int -> Vector Int -> Int
solve2 n v = result $ find p pairs
    where pairs = [(a,b) | a <- [1..100], b <- [1..100]]
          p (a,b) = solve1 a b v == n
          result (Just (a,b)) = a * 100 + b

main :: IO ()
main = do
    input <- readFile "input"
    print $ solve1 12 2 (parse input) -- 3562624
    print $ solve2 19690720 (parse input) -- 8298
