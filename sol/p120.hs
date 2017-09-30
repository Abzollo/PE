-- Notice that (a-1)^n + (a+1)^n mod a^2 = either 1 or 2*n*a (confirm using binomial theorem),
-- where n is odd. Since we want to maximize, we try to maximize 2*n*a such that it's
-- less than a^2, or max 2n s.t. 2n < a.
-- If a is odd, then n = (a-1)/2 would make 2*n*a closest to a^2, so rmax = (a-1)*a
-- If a is even, then n = (a-2)/2 is the max (best we can do given 2n is even as well as a)

main = print sol
sol = sum . map rmax $ a_range
a_range = [3..1000]
rmax a = if odd a then (a-1)*a else (a-2)*a