import Data.List

-- Solution for D<=100,000
-- (9138623307350640837938507246130056541284450249628186652711211526290141593088447074959889691586109808446586788487329855457947449359122812196406183471094085243300140113776722320673519989258197664615506581635543258793642137960637895993371068043922291726707457424732865511811603416709407607285667077345150397643032877987726841522351750655900253781916899660152174806370963781778224969807395941841736594668461346490187409918540644500596164105739747237839541686461147582701486262272823400803243678345216257264359841059899047944315969645240323636858831783554703663860186919961543958325467259751629633944639211198469786177829884186709831569216425531620090438319686256712276877552760998820897069126292769743115626179169131031894821449,92821)
-- 
-- real	0m46.564s
-- user	0m46.314s
-- sys	0m0.206s

-- Shows (max_x, D)
main = print sol
maxD = 1000

-- Maximum minimal x for each D
sol = maximum . (`zip` ds) . map (fst . findXY) $ ds
-- All possible D values (squares are not possible)
ds = ([1..maxD] \\ (map (^2) [1..(floor . sqrt . fromIntegral $ maxD)]))
-- Find minimal solution (x,y) using continued fractions of sqrt(D)
findXY d = if even period
           then sumContFrac . init $ dContFrac  -- Up to period-1
           else sumContFrac . init $ dContFrac ++ tail dContFrac -- Up to 2*period-1
           where dContFrac = getSeqFrac d
                 period = length dContFrac - 1
-- This takes a finite continued fraction sequence and returns the simplified fraction
sumContFrac = foldr (\n r -> sumFrac (n,1) (invFrac r)) (0,0)  -- I know (0,0) is undefined but whatever
-- Functions to sum fractions
sumFrac r (0,0) = r
sumFrac (0,0) r = r
sumFrac (p1,q1) (p2,q2) = (div p g, div q g)
    where q = lcm q1 q2
          p = (p1 * div q q1) + (p2 * div q q2)
          g = gcd p q
invFrac (p,q) = (q,p)
-- Function to get the continued fraction sequence (first element is not in the period)
getSeqFrac n = getSeqFrac' n (0, 1) []
getSeqFrac' n (p,q) old
    | elem (p,q) old = []
    | otherwise = a : getSeqFrac' n (newp,newq) ((p,q) : old)
    where a = floor $ (sqrt (fromIntegral n) + fromIntegral p) / (fromIntegral q)
          newp = q * a - p
          newq = div (n - newp^2) q