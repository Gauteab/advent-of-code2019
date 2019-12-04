-- import Data.List.Split
import Data.List.Extra hiding (union)
import Data.Either
import Data.Maybe
import Data.Char (digitToInt)
-- import Data.List
-- import Data.Set (member, toList, union, fromList, empty, Set, intersection)

-- Ugh
isValid :: String -> Bool
isValid [_]  = True
isValid (x:y:xs)  = p x y && isValid (y:xs) 

p x y = digitToInt x <= digitToInt y

hasDouble :: [Char] -> Bool
hasDouble (x:y:z:xs) | x==y && x==z = hasDouble $ dropWhile (==x) xs
                     | x==y = True
                     | otherwise = hasDouble (y:z:xs)
hasDouble [x, y] = x==y
hasDouble _ = False

solve1 :: Int -> Int -> Int
solve1 start end = length $ filter (\x -> hasDouble x && isValid x) $ map show [start..end]

main :: IO ()
main = do
    print $ hasDouble "123444"
    print $ hasDouble "112233"
    print $ hasDouble "111122"
    print $ solve1 128392 643281
