main = print sol

sol = maximum [x*y | x <- [999,998..100], y <- [999,998..x], palindrome . show $ x*y]
palindrome x = x == reverse x