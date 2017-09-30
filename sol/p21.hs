--Code Trash Can
--fact n = [(c,quot n c) | c <- [2..(limit n)], rem n c == 0]
--sumTups :: [(Int,Int)] -> Int
--sumTups = (+1) . sum . map (\(x,y) -> x+y)
--numsAndFactors :: Int -> [(Int,Int)]
--numsAndFactors n = map (\x -> (x, sumFactors x)) [1..n]

main = print sol
sol = sum $ amicableNums 10000

sumFactors :: Int -> Int
sumFactors n = let c = limit n in if c*c == n then (-c) + sumFactors' n else sumFactors' n
sumFactors' :: Int -> Int
sumFactors' n
    | even n = 1 + sum [c+(div n c) | c <- [2..(limit n)], mod n c == 0]
    | otherwise = 1 + sum [c+(div n c) | c <- [3,5..(limit n)], mod n c == 0]

limit :: Int -> Int
limit = floor . (**0.5) . fromIntegral

amicable :: Int -> Bool
amicable n = let s = sumFactors n in s /= n && n == sumFactors s

amicableNums :: Int -> [Int]
amicableNums n = filter amicable [2..n]