import Data.List (sort)

main = print sol
d = 9
sol = fst . head . filter (panDigitalLast . fib . fst) . filter (panDigitalFirst . snd) $ (zip [0..] fibs)
panDigitalFirst n = (sort . show . mod n $ 10^d) == "123456789"
panDigitalLast n = (sort . take d . show $ n) == "123456789"

fibs = 0 : 1 : zipWith (addModulo (10^d)) fibs (tail fibs)
    where addModulo m x y = mod (x + y) m

fib n = fst $ fib2 (n-1)
fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
 | even n    = (a*a + b*b, c*c - a*a)
 | otherwise = (c*c - a*a, b*b + c*c)
 where (a,b) = fib2 (n `div` 2 - 1)
       c     = a + b

-- Starightforward solution
sol' = fst . head . filter (panDigital . snd) $ (zip [0..] fibs')
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')
panDigital n = panDigitalFirst n && panDigitalLast n