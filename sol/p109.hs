main = print sol
sol = sum $ map validPlays [1..99]

validFinalMove n = even n && 1 <= n && n <= 40 || n == 50
validPlays n = length $ filter (validFinalMove . (n-)) twoMoves

moves = 0 : 25 : 50 : [m*p | m <- [1..3], p <- [1..20]]
twoMoves = map sum (pairs moves)

pairs [] = []
pairs (x:ys) = map (\y -> [x,y]) (x:ys) ++ pairs ys