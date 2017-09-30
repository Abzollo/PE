import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List (tails)

main = print sol
sol = last . last . filter (not . null) $ primeSums 1000
primeSums end = map (filter isPrime . takeWhile (<10^6) . consecSums) [21,23..end]
consecSums many = map (sum . take many) . tails $ primes

-- Shows the consecutive primes
sol' = head . map (take 543) . filter ((==997651) . sum . take 543) . tails $ primes