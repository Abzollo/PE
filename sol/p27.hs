import Math.NumberTheory.Primes.Testing (isPrime)
import Math.NumberTheory.Primes.Sieve (primes)

main = let (_,a,b) = sol in print (a*b)

upper = 1000
est = 100

sol = maximum $ quadPrimes' myPrimes [1,3..est]
myPrimes = reverse $ takeWhile (<=upper) primes
quadPrimes' [] _ = []
quadPrimes' (b:bs) p = maximum (quadPrimes b p) : quadPrimes' bs p

quadPrimes _ [] = []
quadPrimes b (a:as) = s : quadPrimes b as
    where s1 = numPrimes b a
          s2 = numPrimes b (-a)
          s = if s1 > s2 then (s1,a,b) else (s2,-a,b)
    
numPrimes b a = toInteger . length $ takeWhile (\n -> isPrime (n^2+a*n+b)) [0..]