main = print sol
sol = solvep1 1000
solvep1 up = sum [x | x <- [3 .. (up - 1)], mod x 3 == 0 || mod x 5 == 0]