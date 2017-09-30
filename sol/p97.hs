main = print sol
-- A cheap solution
sol' = mod (1 + 28433*2^7830457) (10^10)
-- A little bit more effort
sol = mod (1 + 28433 * fastPower 2 7830457) (10^10)
fastPower x n
    | n == 1 = x
    | odd n = x * (fastPower x (n `div` 2))^2 `mod` (10^10)
    | otherwise = (fastPower x (n `div` 2))^2 `mod` (10^10)