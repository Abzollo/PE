import Data.Char (digitToInt)

main = print $ sol
sol = let (p,_) = euler 100 in sum . map digitToInt . show $ p

fromFrac (p,q) = (fromIntegral p) / (fromIntegral q)
sumFrac r (0,0) = r
sumFrac (0,0) r = r
sumFrac (p1,q1) (p2,q2) = (div p g, div q g)
    where q = lcm q1 q2
          p = (p1 * div q q1) + (p2 * div q q2)
          g = gcd p q
invFrac (p,q) = (q,p)

euler acc = (2,1) `sumFrac` (invFrac $ euler' 2 acc)
euler' n acc
    | n <= acc = (p,1) `sumFrac` (invFrac $ euler' (n+1) acc)
    | otherwise = (0,0)
    where p = if mod n 3 == 0
                then 2 * div n 3
                else 1
