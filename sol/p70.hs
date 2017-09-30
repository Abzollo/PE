import Math.NumberTheory.Primes.Factorisation
import Data.List (sort)

main = print sol
maxN = 10^7

sol = snd . minimum . zip (map (\(n,m) -> (fromIntegral n) / (fromIntegral m)) specialTotients) $ (map fst specialTotients)

specialTotients = filter (\(n, m) -> isPermOf n m) . zip [2..maxN] . map (sieveTotient . totientSieve $ maxN) $ [2..maxN]

isPermOf n m = (sort . show $ n) == (sort . show $ m)