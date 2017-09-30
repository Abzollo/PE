import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Math.NumberTheory.Primes.Factorisation (tau)

main = print sol1

-- tau is much faster than my stupid goodDiv
sol1 = head $ dropWhile (\x -> tau x <= 500) [n*(n+1) `div` 2 | n <- [3,5..], not $ isPrime n, not $ isPrime (n+1)]

sol2 = head $ [x*(x+1) `div` 2 | x <- non2Primes, goodDiv x > 500]
non2Primes = [x | x <- [3,5..], not $ isPrime x, not $ isPrime (x+1)]
goodDiv x
    | even x = divCount (x `div` 2) * divCount (x+1)
    | otherwise = divCount x * divCount ((x+1) `div` 2)
divCount n = divvy n primes
divvy 1 _ = 1
divvy n (p:rest) = let k = numDivs n p in (k+1) * divvy (n `div` p^k) rest
numDivs n p
    | n `mod` p == 0 = 1 + numDivs (n `div` p) p
    | otherwise = 0
