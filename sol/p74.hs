
-- Brute force runs under a minute. Cool.
-- real	0m57.391s
-- user	0m56.838s
-- sys	0m0.523s

main = print sol

sol = length . filter ((==60) . snd) . zip [1..maxN] . map lenOfFactorialCycle $ [1..maxN]

maxN = 10^6

lenOfFactorialCycle :: Int -> Int
lenOfFactorialCycle n = length $ factorialCycle n

factorialCycle :: Int -> [Int]
factorialCycle n = factorialCycle' n []

factorialCycle' :: Int -> [Int] -> [Int]
factorialCycle' n prev
    | n `elem` prev = prev
    | otherwise = factorialCycle' (sumDigitsFactorial n) (n : prev)
    

sumDigitsFactorial :: Int -> Int
sumDigitsFactorial = sum . factorialOfDigits

factorialOfDigits :: Int -> [Int]
factorialOfDigits n
    | n > 0 = factorial digit : factorialOfDigits remaining
    | otherwise = []
    where (remaining, digit) = n `divMod` 10

factorial :: Int -> Int
factorial n = product [2..n]