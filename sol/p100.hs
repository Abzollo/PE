import Data.List (find)

main = print sol
sol = snd <$> find ((>target) . fst) validBalls
target = 10^12

-- Generate solutions (x,y) to x^2 - 2*y^2 = -1 with some continued fractions hack
validBalls = map (getBlueBalls . completeSqrt2) $ iterate deeperDenom (0,1)  -- p(0) = 0
getBlueBalls (x,y) = (div (x+1) 2, div (y+1) 2)  -- (Total balls, blue balls)
completeSqrt2 p = (1,1) `sumFrac` invFrac p  -- 1 + 1/p(n)
deeperDenom p = (2,1) `sumFrac` invFrac ((2,1) `sumFrac` invFrac p)  -- p(n) = 2 + 1/(2 + p(n-1))

-- Functions to sum fractions
sumFrac r (0,_) = r
sumFrac (0,_) r = r
sumFrac (p1,q1) (p2,q2) = (div p g, div q g)
    where q = lcm q1 q2
          p = (p1 * div q q1) + (p2 * div q q2)
          g = gcd p q
invFrac (0,_) = (0,1)
invFrac (p,q) = (q,p)