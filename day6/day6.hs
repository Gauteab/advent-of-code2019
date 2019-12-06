import Data.List.Extra (splitOn)
import Data.Map (Map)
import qualified Data.Map as M 

type Node = String
type Graph = Map Node [Node]

parse :: String -> Graph
parse = foldr (M.unionWith (++)) M.empty . fmap (f . splitOn ")") . lines
    where f [key, value] = M.singleton key [value]

orbits :: Graph -> Int
orbits = undefined

depthFrom :: Int -> Graph -> Node -> Int
depthFrom acc graph node = maybe acc f $ M.lookup node graph
    where f = (+ acc) . sum . fmap (depthFrom (acc + 1) graph)

main :: IO ()
main = do
    input <- readFile "input"
    print $ parse input
    print $ depthFrom 0 (parse input) "COM"
