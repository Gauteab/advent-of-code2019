import Data.List.Split
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Data.Vector ((//), Vector, (!))
import Data.Function

parse :: String -> Vector Int
parse = V.fromList . fmap read . splitOn ","

type State = (Int, Vector Int)

execute :: (Int -> Int -> Int) -> Int -> Vector Int -> Vector Int
execute op i v = v // [(v!(i+3), op (v!(v!(i+1))) (v!(v!(i+2))))]

step :: State -> State
step (i, v) = 
    case v!i of
         99 -> (i, v)
         1 -> (i+4, execute (+) i v)
         2 -> (i+4, execute (*) i v)

programTrace :: State -> [State]
programTrace = iterate step 

runProgram :: State -> State
runProgram = fst
           . fromJust
           . find (\(s1, s2) -> fst s1 == fst s2) 
           . (zip <*> tail )
           . programTrace 

main :: IO ()
main = do
    input <- readFile "input"
    let input' = parse input
    print $ runProgram (0, input') -- 3562624
    -- print $ solve2 19690720 (parse input) -- 8298
