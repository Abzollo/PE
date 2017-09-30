-- 4782
-- (22.06 secs, 6,805,119,648 bytes)
-- real	0m2.565s
-- user	0m2.528s
-- sys	0m0.027s

main = print sol
sol = fibDigits 1000

fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib' n 1 1

fib' 2 p1 p2 = p2
fib' n p1 p2 = fib' (n-1) p2 (p1+p2)

fibDigits l = let big = 10^(l-1) in head [n | n <- [1..], div (fib n) big > 0]