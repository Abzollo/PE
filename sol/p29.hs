import qualified Data.Set as S

main = print sol

sol = specialNum + 99*(length $ filter (\x -> not $ elem x dontdothese) [2..100])
special = [2,3,5,6,7,10]
dontdothese = S.fromList . concat $ map getPows special

specialNum = sum $ map (findPow' 2 100 . length . getPows) special
getPows n = takeWhile (<=100) $ map (n^) [1..]

-- Using actual numbers
findPow start end num c = length . S.fromList . concat . (take c) $ map (pows start end) (map (num^) [1..])
pows start end n = [n^x | x <- [start..end]]

-- Just using the number of factors to represent the numbers themselves
findPow' start end c = length . S.fromList . concat . (take c) $ map (pows' start end) [1..]
pows' start end n = [n*x | x <- [start..end]]