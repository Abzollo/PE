-- Just (55374,36325300925435785930832331577396761646715836173633893227071086460709268608053489541731404543537668438991170680745272159154493740615385823202158167635276250554555342115855424598920159035413044811245082197335097953570911884252410730174907784762924663654000000)

-- real	0m1.859s
-- user	0m1.821s
-- sys	0m0.026s

import Data.Array.IArray
import Data.List (find)

main = print sol
maxN = 10^5  -- Will run for a long time, until target is found (or not)
targetDivisor = 10^6
sol = find ((==0) . (`mod` targetDivisor) . snd) . zip [1..maxN] $ allPartitions
allPartitions = map (partitionArray maxN !) [1..maxN]

partitionArray n = dpPartitionArray
    where dpPartitionArray :: Array Int Integer
          dpPartitionArray = listArray (0, n) [dpPartition n' | n' <- [0..n]]
          dpPartition 0 = 1
          dpPartition n = sum . map (f n) $ [1..maxK]  -- We stop at maxK because the rest are 0s
          maxK = floor $ (1 + sqrt' (1+24*n)) / 6  -- Max integer k s.t. n1 >= 0 (see below)
          f n k
              | n1 < 0 = 0
              | n2 < 0 = sign * dpPartitionArray ! n1
              | otherwise = sign * (dpPartitionArray ! n1 + dpPartitionArray ! n2)
              where m = k*(3*k-1) `div` 2
                    n1 = n - m
                    n2 = n1 - k
                    sign = if odd k then 1 else -1

sqrt' = sqrt . fromIntegral