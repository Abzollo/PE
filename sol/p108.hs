-- Number of divisors of n^2 from factorizing n.
-- Take the exponents of each prime in the prime factorization,
-- multiply by 2, add 1, take their products.

import Math.NumberTheory.Primes.Factorisation

main = print sol
target = 10^3
maxN = 10^6
fs = factorSieve maxN

sol = head . filter ((>target) . (\s -> div (s+1) 2) . numOfDivisorsOfSq)  $ [1..maxN]
numOfDivisorsOfSq = product . map ((\a -> 2*a+1) . snd) . sieveFactor fs

-- Just for checking solution
findXY n = map (\k -> (n+k, n + div (n^2) k)) $ findK n
findK n = filter (<=n) $ allDivisors (n^2)

allDivisors = allDivisors' . sieveFactor fs
allDivisors' = map product . sequenceA . map (\(p,m) -> map (p^) [0..m])