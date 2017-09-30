main = print sol

sol = solvep6 100

sumSq l = sum $ map (^2) l
sumSqTo up = up * (up+1) * (2*up+1) / 6
sqSum l = (sum l)^2
sqSumTo up = (up * (up+1) / 2)^2
solvep6 up = sqSum [1..up] - sumSq [1..up]
solvep6' up = sqSumTo up - sumSqTo up