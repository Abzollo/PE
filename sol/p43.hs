-- ghc -O2
-- 16695334890
--
-- real	0m24.427s
-- user	0m24.107s
-- sys	0m0.274s

import Data.List (permutations)

main = print $ sum sol
sol = filter subStrDivisible . map read $ permutations "0123456789"

myprimes = [2,3,5,7,11,13,17]
subStrDivisible num = 0 == (sum $ zipWith (mod) (getDigits num) myprimes)
getDigits num = map (\d -> num `div` 10^(10-d) `mod` 10^3) [4..10]