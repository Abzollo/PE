import Math.NumberTheory.Primes (primes)
-- Didn't bother to write down the primes, ugh too much work.

main = print sol
sol = findN 20  -- Try bigger partitions incrementally until solution stabilizes
target = 4*10^6

fromPowers = product . zipWith (^) primes
numOfDivOfSq = product . map (\a -> 2*a+1)  -- Let's call it k
ns_and_ks = map (\powers -> (fromPowers powers, numOfDivOfSq powers)) . partition
findN maxParts = minimum . filter (\(_,k) -> k > 2*target-1) $ concatMap ns_and_ks [1..maxParts]

partition n = parts n n
parts 0 _ = [[]]
parts s maxS = concat [map (nextS:) $ parts (s-nextS) nextS | nextS <- [1..maxS], s-nextS >= 0]

