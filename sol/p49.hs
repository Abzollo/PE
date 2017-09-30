-- (+0450) : 0163, 0613, 1063.
-- (+3330) : 1487, 4817, 8147.
-- (+3330) : 2969, 6299, 9629.
-- (+3330) : 0379, 3709, 7039.


import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List (permutations, nub, sort, tails)

main = print sol
sol = zipWith getSeq additions finalists
getSeq s (a:t) = if elem (a+s) t then (a,a+s,a+s+s) else getSeq s t
additions = map (getDup . concat . filter unusual . differences) finalists

finalists = nub . filter (any unusual . differences) $ good candidates
good = filter ((>2) . length) . map (sort . filter isPrime . nub . map read . permutations . show)
candidates = takeWhile (<10000) . dropWhile (<1000) $ primes

unusual [] = False
unusual (a:t) = if elem a t then True else unusual t
getDup (a:t) = if elem a t then a else getDup t
    
differences = concat . map diff . tails
diff [] = []
diff [_] = []
diff (a:t) = filter (not . null) . map (subdiff a) $ tails t
subdiff _ [] = []
subdiff a (b:t) = (b-a) : map (\c -> c-b) t