import Data.List (sort)

-- After noticing the pattern (p1 + p2*k) / (q1 + q2*k), we can solve this problem by hand
-- We first know that p1/q1 < p2,q2
main = print . (\d -> (p1 + p2*div d q2, d + q1)) . head . dropWhile ((>0) . (`mod` q2)) $ [maxD-q1, maxD-q1-1..]
    where p1 = 2
          q1 = 5
          p2 = 3
          q2 = 7
maxD = 10^100 -- mathalan

-- Investigation
main' = print . (takeWhile (/= (3,7))) . (dropWhile (/= (2,5))) $ sol

d = 30
sol = map snd . sort . map (\r -> (fromFrac r, r)) . concat . map allFracs $ [2..d]

allFracs q = [(p,q) | p <- [1..q], gcd p q == 1]

sumContFrac :: [Integer] -> (Integer, Integer)
sumContFrac = foldr (\n r -> sumFrac (n,1) (invFrac r)) (0,0)  -- I know (0,0) is undefined but whatever
-- Functions to sum fractions
fromFrac (p,q) = (fromIntegral p) / (fromIntegral q)
sumFrac r (0,0) = r
sumFrac (0,0) r = r
sumFrac (p1,q1) (p2,q2) = (div p g, div q g)
    where q = lcm q1 q2
          p = (p1 * div q q1) + (p2 * div q q2)
          g = gcd p q
invFrac (p,q) = (q,p)