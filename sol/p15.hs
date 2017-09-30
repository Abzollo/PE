main = print sol1
sol1 = fastsol 20
sol2 = latticepath 20
latticepath n = 2 * latpath n (n-1)
latpath _ 0 = 1
latpath 0 _ = 1
latpath x y
    | x == y = latticepath x
    | otherwise = latpath x (y-1) + latpath (x-1) y


-- Or use this (much faster)
fastsol n = pfact (2*n) n / fact n
pfact n k
    | n == k = 1
    | otherwise = n * pfact (n-1) k
fact n = pfact n 1