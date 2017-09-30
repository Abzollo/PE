import Math.NumberTheory.Primes.Sieve
import Data.Set (fromList, size)

main = print sol
target = 5 * 10^7
primesLT n = takeWhile (<=n) primes
sol = (size . fromList) [n | p2 <- primesLT (sqrt2 target),
           p3 <- primesLT (sqrt3 target),
           p4 <- primesLT (sqrt4 target),
           let n = p2^2 + p3^3 + p4^4,
           n <= target]


sqrt2 = floor . sqrt . fromIntegral
sqrt3 = floor . (**(1/3)) . fromIntegral
sqrt4 = floor . sqrt . sqrt . fromIntegral
