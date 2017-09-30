
main = print $ length sol
sol = filter numBigDen . map sqrt2 $ [1..1000]
numBigDen (p,q) = length (show p) > length (show q)
fromFrac (p,q) = (fromIntegral p) / (fromIntegral q)
sumFrac (p1,q1) (p2,q2) = (div p g, div q g)
    where q = lcm q1 q2
          p = (p1 * div q q1) + (p2 * div q q2)
          g = gcd p q
invFrac (p,q) = (q,p)
sqrt2 acc = (1,1) `sumFrac` invFrac (sqrt2Coeff acc)
sqrt2Coeff 1 = (2,1)
sqrt2Coeff a = (2,1) `sumFrac` (invFrac $ sqrt2Coeff (a-1))