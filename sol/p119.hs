import Data.List (sort)

main = print sol
sol = (sort candidates) !! (target - 1)
target = 30
limit = 10**10
candidates = [x^n | n <- [2..100], x <- [2..100], x == sumDigits (x^n)]
sumDigits = sum . digitize
digitize 0 = []
digitize n = let (q,r) = divMod n 10 in r : digitize q