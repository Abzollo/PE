import Data.List (find)

main = print sol

target = 10^6
maxM = 5000
sol = find ((>target) . cuboids) [1..]

cuboids m = sum $ map (\(a,b,_) -> countLegal m a b) triplets

countLegal m a b = if a > b then countLegal' m a b else countLegal' m b a
countLegal' m a b
    | b > m = 0
    | a > m && b >= lowb =  (b - (a-b)) `div` 2 + 1
    | a > m && b < lowb = 0
    | b >= lowb  = b `div` 2 + (b - (a-b)) `div` 2 + 1
    | otherwise = b `div` 2
    where lowb = a `div` 2 + if odd a then 1 else 0

triplets = concat . filter (not . null) . map findTriplets $ [1..3*maxM]

-- Slow
sol' = length [0 | x <- [1..maxM], y <- [x..maxM], z <- [y..maxM], isInt (shortestWallPath z x y)]
shortestWallPath a b c = sqrt . fromIntegral $ a^2 + (b+c)^2
isInt x = x == fromIntegral (floor x)

-- For finding triplets quickly
findTriplets s2 = concat $ map (findTripletsAtM s2) (findMs s2)  -- Note s2 = s/2

findMs :: Int -> [Int]
findMs s2 = filter (\m -> mod s2 m == 0) [2 .. (sqrt' s2)-1]

findTripletsAtM :: Int -> Int -> [(Int, Int, Int)]
findTripletsAtM s2 m = let s_reduced = remove2s (s2 `div` m)
                           k0 = m + 1 + mod m 2  -- k0 is the smallest odd integer > m
                       in [getTriplet s2 m k | k <- [k0, k0+2 .. min (2*m-1) s_reduced],
                                               mod s_reduced k == 0, gcd k m == 1]

getTriplet :: Int -> Int -> Int -> (Int, Int, Int)
getTriplet s2 m k = (d*(m*m - n*n), 2*d*m*n, d*(m*m + n*n))
              where d = s2 `div` (k*m)
                    n = k-m

remove2s x
    | rem == 0 = remove2s x2
    | otherwise = x
    where (x2, rem) = x `divMod` 2

sqrt' = ceiling . sqrt . fromIntegral