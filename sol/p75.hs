-- Using an array for all s values, transforming primitive triplet (3,4,5) to other primitives
-- or multiplying them to get imprimitives, we can count triplets for each s much faster.
-- Consider only even s values to reduce storage by 2.
-- This is difficult to do in Haskell, so fuck it. This solution runs in under a minute, though.

main = print sol
maxL = 1500000  -- 1,500,000

sol = length . filter ((==1) . length) . map findTriplets $ [1..div maxL 2]
sol' = filter (not . null . snd) . zip [2,4..maxL] . map findTriplets $ [1..div maxL 2]

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