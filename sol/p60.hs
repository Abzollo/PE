import Math.NumberTheory.Primes (primes, isPrime)
import Data.List (tails, minimumBy)
import Data.Ord (comparing)

main = mapM_ print ["Primes: " ++ show sol, "Solution: " ++ show (sum sol)]
sol = minimumBy (comparing sum) $ candidates 5

filterCands = concat . map goodPrimesList . init . tails
candidates n = iterate filterCands (map (:[]) . takeWhile (<10000) $ primes) !! (n - 1)

goodPrimesList (n:ns) = map ((n++) . (:[]) . last) . filter (primeConcatList (reverse n) . reverse) $ ns
primeConcatList (n1:ns1) (n2:ns2) = ns1 == ns2 && goodPrimes n1 n2

goodPrimes n1 n2 = isPrime (concatNum n1 n2) && isPrime (concatNum n2 n1)
concatNum n1 n2 = read $ show n1 ++ show n2 :: Integer
