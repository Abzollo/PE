import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List (sort, nub)
import Data.Char (digitToInt)

main = print $ head (head sol)
sol = findSolFor 6
findSolFor d = nub . map (multiples d) . filter (property d) $ [1..1000000]
property d n = allSame . map (sort . digits . (n*)) $ [1..d]
multiples d n = map (n*) $ [1..d]
digits = map digitToInt . show
allSame x = and $ map (== head x) (tail x)