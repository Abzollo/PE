import Math.NumberTheory.Primes.Sieve (primes)
import Data.Array.IArray

main = print sol
sol = head . dropWhile ((<=lowerBound) . snd) . zip [1..maxN] . map (\n -> coins' n (n-1)) $ [1..maxN]
maxN = 300  -- A very generous estimate
lowerBound = 10^6  -- An overkill boundary

ps = map fromIntegral $ take maxN primes

pArray :: Array Int Int
pArray = listArray (1,maxN) ps

-- Array of indices s.t. pIndArray[n] = i iff p_i is the biggest prime <= n
pIndArray :: Array Int Int
pIndArray = listArray (1,maxN) $ (0 : pInds 2 1)
pInds n i
    | n > maxN = []
    | n >= pArray ! (i+1) = (i+1) : pInds (n+1) (i+1)
    | otherwise = i : pInds (n+1) i

-- This time, this is sufficient and runs fast enough for 5,000
coins n i
    | i == 0 = 0  -- Ran out of primes
    | n == 0 = 1  -- Successfully partitioned n
    | p_i > n = coins n (pIndArray ! n)  -- Can only partition n using primes <= n
    | otherwise = coins (n-p_i) i + coins n (i-1)
    -- Either use prime #i to partition n, or skip prime #i and check again for #i-1
    where p_i = pArray ! i

-- Using DP, we can solve quickly up to 1,000,000
coins' n i = dpCoins n i
    where dpCoinsArray :: Array (Int, Int) Integer
          dpCoinsArray = listArray ((0, 0), (n, i)) [dpCoins n' i' | n' <- [0..n], i' <- [0..i]]
          dpCoins n i
              | i == 0 = 0
              | n == 0 = 1
              | p_i > n = dpCoinsArray ! (n, pIndArray ! n)
              | otherwise = dpCoinsArray ! (n-p_i, i) + dpCoinsArray ! (n, i-1)
              where p_i = pArray ! i