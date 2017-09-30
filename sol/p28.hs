-- 669171001
-- (0.01 secs, 2,120,944 bytes)

main = print sol
sol = 1 + sum [4*(2*n-1)^2 + 10*2*n | n <- [1..500]]