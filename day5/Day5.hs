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

decode :: Int -> (Int, Int, Int, Int)
decode code = (rem code 100, f 5, f 4, f 3)
    where f x =  code `div` 10^(x-1) `rem` 10

eval :: Int -> Int -> Vector Int -> Int
eval mode index vec = 
    let f = bool id (vec !) (mode == 0)
    in  f index

eval' :: Int -> Int -> Vector Int -> Int
eval' mode index vec = eval mode (vec!index) vec

arithmetic :: (Int -> Int -> Int) -> Int -> Int -> Int -> Vector Int -> State
arithmetic f b c i v = (i+4, v // [(a', f b' c')])
    where a' = v ! (i+3)
          b' = eval' b ((i+2)) v
          c' = eval' c ((i+1)) v

output c i v = trace (show (eval' c (i+1) v)) v

input :: Int -> Int -> Vector Int -> Vector Int
input c i v = v // [(eval c (i+1) v, 5)]

jumpIf :: Int -> Int -> Int -> Int -> Vector Int -> State
jumpIf x b c i v = (if (eval' c (i+1) v) == x then (eval' b (i+2) v) else (i+3), v)

comp :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Int -> Vector Int -> State
comp p a b c i v = 
    let result = bool 0 1 (p (eval' c (i+1) v) (eval' b (i+2) v))
    in  (i+4, v // [(v!(i+3), result)])

step' s = trace (show ((\(i,v) -> (i,V.take 10 v)) s)) (step s)
step :: State -> State
step (i, v) = 
    let (op, a, b, c) = decode (v!i)
    -- in case trace ("op: " ++ show op ++ show (decode (v!i))) op of
     in case op of
         1 -> arithmetic (+) b c i v
         2 -> arithmetic (*) b c i v
         3 -> (i+2, input c i v)
         4 -> (i+2, output c i v)
         5 -> jumpIf 1 b c i v
         6 -> jumpIf 0 b c i v
         7 -> comp (<) a b c i v
         8 -> comp (==) a b c i v
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
    print $ decode 99999
    -- print $ runProgram (0, parse "3,0,4,0,99")
    print $ runProgram initialState
    -- print $ runProgram (0, input') -- 3562624
    -- print $ solve2 19690720 (parse input) -- 8298
    return ()
