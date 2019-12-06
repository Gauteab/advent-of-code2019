import Data.List.Extra (splitOn)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M 
import qualified Data.Set as S 

type Node = String
type Graph = Map Node [Node]
data GraphType = Directed | Undirected

parse :: GraphType -> String -> Graph
parse graphType = foldr (M.unionWith (++)) M.empty . fmap (f graphType . splitOn ")") . lines
    where f Directed   [n1, n2] = M.singleton n1 [n2]
          f Undirected [n1, n2] = M.fromList [(n1, [n2]), (n2, [n1])]

getEdges :: Node -> Graph -> [Node]
getEdges node graph = concat $ M.lookup node graph

-- Part one
orbitCountChecksum :: Int -> Graph -> Node -> Int
orbitCountChecksum acc graph node = f $ getEdges node graph
    where f = (+ acc) . sum . fmap (orbitCountChecksum (acc + 1) graph)

-- Part two
distanceToSanta :: Node -> Graph -> Int -> Node -> Int
distanceToSanta previous graph count node  = 
    if isGoal then 
        count - 1 
    else 
        sum $ distanceToSanta node graph (count+1) <$> edges
    where 
        edges = filter (/= previous) $ getEdges node graph
        isGoal = any ("SAN" ==) $ getEdges node graph

main :: IO ()
main = do
    input <- readFile "input"
    print $ orbitCountChecksum 0 (parse Directed input) "COM"
    print $ distanceToSanta "YOU" (parse Undirected input) 0 "YOU"
