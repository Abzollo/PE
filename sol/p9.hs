main = print sol

sol = product . head . rightTriangles' $ 1000

rightTriangles s = [[a,b,c] | c <- [1..500], b <- [1..c], a <- [1..b], a+b+c == s, c^2 == a^2 + b^2]
rightTriangles' s = [[a,b,s-a-b] | a <- [1..(s-3)/3], b <- [1..(s-a)/2], a^2 + b^2 == (s-a-b)^2]