
fuelRequirement = subtract 2 . flip div 3
fuelRequirement2 mass = sum $ takeWhile (>0) $ iterate fuelRequirement (fuelRequirement mass)

solve1 :: String -> Int
solve1 = sum . map (fuelRequirement . read) . words

solve2 :: String -> Int
solve2 = sum . map (fuelRequirement2 . read) . words

main :: IO ()
main = do
    content <- readFile "input"
    print $ solve1 content -- 3295424
    print $ solve2 content -- 4940279

