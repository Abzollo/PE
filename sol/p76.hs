-- Without DP (maxN = 100)
-- 190569291 
-- real	1m18.907s
-- user	1m18.254s
-- sys	0m0.551s
--
--
-- With DP (maxN = 1000)
-- 24061467864032622473692149727990
-- real	0m0.716s
-- user	0m0.632s
-- sys	0m0.067s

import Data.Array.IArray

main = mapM_ print sol
maxN = 1000
sol = [coins' maxN (maxN-1)]

coins n c
    | n == 0 = 1         -- There is one way to make a 0.
    | c == 1 = 1         -- If the largest coin we can use is 1, then there is one way.
    | c > n = coins n n  -- We can only use coins as large as the one we want to partition
    | otherwise = coins (n-c) c + coins n (c-1)
    -- Either use coin #c to partition n, or skip coin #c and check again for #c-1

coins' n c = dpCoins n c
    where dpCoinsArray :: Array (Int, Int) Integer
          dpCoinsArray = listArray ((0, 1), (n, c)) [dpCoins n' c' | n' <- [0..n], c' <- [1..c]]
          dpCoins n c
              | n == 0 = 1
              | c == 1 = 1
              | c > n = dpCoinsArray ! (n, n)
              | otherwise = dpCoinsArray ! (n-c, c) + dpCoinsArray ! (n, c-1)