main = print sol
sol = solvep10 2000000
solvep10 limit = sum . (takeWhile (< limit)) $ primes
primes = 2 : filter (null . tail . primeFactors) [3,5..]
primeFactors n = factor n primes
    where
      factor n (p:ps)
        | p*p > n = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise = factor n ps