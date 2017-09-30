import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List (permutations)

main = print sol
sol = maximum . filter (isPrime . read) . permutations $ "7654321"