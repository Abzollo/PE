
main = print $ length sol
sol = filter lychrel [1..10000]
isPalindrome x = (show x) == reverse (show x)
lychrel x = (==51) . length . take 51 . takeWhile (not . isPalindrome) . tail . iterate reverseSum $ x
reverseSum n = let m = read . reverse . show $ n in  n+m