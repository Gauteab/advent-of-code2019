import Data.List.Split
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Data.Vector ((//), Vector, (!))
import Data.Function
import Data.Bool
import Debug.Trace

parse :: String -> Vector Int
parse = V.fromList . fmap f . splitOn ","
    where f ('-':xs) = -(read xs)
          f x = read x

type State = (Int, Vector Int)

execute :: (Int -> Int -> Int) -> Int -> Vector Int -> Vector Int
execute op i v = v // [(v!(i+3), op (v!(v!(i+1))) (v!(v!(i+2))))]

decode :: Int -> (Int, Int, Int, Int)
decode code = (rem code 100, f 5, f 4, f 3)
    where f x =  code `div` 10^(x-1) `rem` 10

eval :: Int -> Int -> Vector Int -> Int
eval mode index vec = 
    let f = bool id (vec !) (mode == 0)
    in  f index

arithmetic :: (Int -> Int -> Int) -> (Int, Int, Int) -> Int -> Vector Int -> Vector Int
arithmetic f (a, b, c) i v = v // [(a', f b' c')]
    where a' = eval a (i+3) v
          b' = eval b (v!(i+2)) v
          c' = eval c (v!(i+1)) v

output c i v = trace (show (eval c (v!(i+1)) v)) v

input :: Int -> Int -> Vector Int -> Vector Int
input c i v = v // [(eval c ((i+1)) v, 1)]

step' s = trace (show ((\(i,v) -> (i,V.take 10 v)) s)) (step s)
step :: State -> State
step (i, v) = 
    let (op, a, b, c) = decode (v!i)
    in case trace ("op: " ++ show op ++ show (decode (v!i))) op of
     -- in case op of
         1 -> (i+4, arithmetic (+) (a, b, c) i v)
         2 -> (i+4, arithmetic (*) (a, b, c) i v)
         3 -> (i+2, input c i v)
         4 -> (i+2, output c i v)
         99 -> (i, v)

programTrace :: State -> [State]
programTrace = iterate step' 

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
    -- print $ runProgram (0, parse "1002,4,3,4,33")
    let initialState = (0, input')
    -- print $ decode 12302
    -- print $ runProgram (0, parse "3,0,4,0,99")
    print $ runProgram initialState
    -- print $ runProgram (0, input') -- 3562624
    -- print $ solve2 19690720 (parse input) -- 8298
    return ()
