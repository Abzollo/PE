--
-- Let's find a triangle (a,a,a+1) or (a,a,a-1) such that its area is integral.
-- This problem is equivalent to finding a such that (a+1)(3a-1) is a perfect square or
-- (a-1)(3a+1) is a perfect square.
-- There are two types of a:
--  1) a such that (a+1)(3a-1) is a perfect square, we will call those primal a's because
--     I initially thought they were primes and I don't have a better name.
--  2) a such that (a-1)(3a+1) is a perfect square. We call those transformable a's because
--     we are able to generate more a's from them recursively.
--
-- Tranformation:
--  We can prove that if (a-1)(3a+1) = 3a^2 - 2a - 1 = k^2 for some k, then we can show
--  (a-1)(3a+1)(3a-1)^2 = 3(3a^2-2a)^2 - 2(3a^2-2a) - 1 = k^2 * (3a-1)^2.
--  so we can see that x = (3a^2-2a) is a new value that satisfies (x-1)(3x+1) = y^2, for
--  for some y, and so on. We can similarly transform primal a's by (3a^2+2a). Finally,
--  we can see that any transformed a is actually transformable, so we can recursively
--  transform a to generate more a's, hence this problem is reduced to finding primal a's.
--
-- Finding Primal a's:
--  This is a tricky part because I wasn't able to prove my findings, but it works anyways.
--  I found out that for any primal a, a+1 = 2*x^2, and 3a-1 = 2*y^2, for some x and y.
--  It's obvious that they are even, but I don't know why they are always a perfect square.
--  Maybe this is generally false, but it is true for a < 10^9 / 3. Finding the value a
--  now became a much less daunting task. We can find a value y such that it satisfies the
--  property a+1 = 2*x^2. In other words, find y such that (y^2 + 2)/3 = x^2.
--
--  After that, finding whether the third side is a+1 or a-1 is kind of trivial.
--
-------------------------------------------------
-- My stupid and weird solution, runs in ~0.03s --
-------------------------------------------------
import Data.List (sort)
main = print sol
target = div (10^9) 3
sol = sum . map perimeter . tail . sort $ primal_a ++ transformed_a
perimeter a = if is_primal a then 3*a-1 else 3*a+1
    where is_primal a = isSquare $ (a+1)*(3*a-1)

transform a = 3*a*a-2*a
transformPrimal a = 3*a*a+2*a
primal_a = takeWhile (<target) [a | y <- [1..], let a = div (2*y*y+1) 3, isSquare $ div (a+1) 2]
transformable_a = map transformPrimal primal_a
transformed_a = concat . map (takeWhile (<target) . recursiveTransform) $ transformable_a
recursiveTransform a = a : recursiveTransform (transform a)

isInt x = x == fromIntegral (floor x)
isSquare x = x == (sqrt' x)^2
sqrt' = floor . sqrt . fromIntegral

--------------------------------------------------------------
-- A more elegant solution but slightly slower, runs in ~5s --
--------------------------------------------------------------
sol' = sum . map almostT $ ppt (10^9)
    where almostT (a,_,c) = if c-2*a ` elem` [-1,1] then 2*c+2*a else 0
ppt n = concat .takeWhile (not . null) . iterate (concat . map (transformPPT n)) $ [(3,4,5)]
transformPPT n (a,b,c) = filter (\(a,b,c) -> a+b+c <= n)
                           [fixOrder (a-2*b+2*c,  2*a-b+2*c,  2*a-2*b+3*c),
                            --fixOrder (a+2*b+2*c,  2*a+b+2*c,  2*a+2*b+3*c),
                            fixOrder (-a+2*b+2*c, -2*a+b+2*c, -2*a+2*b+3*c)]
    where fixOrder (a,b,c) = if a <= b then (a,b,c) else (b,a,c)

-------------------------------
-- Brute force, runs in ~70s --
-------------------------------
sol'' = sum [if isSquare (r1*r2) then r2 else s2 | a <- [3,5..target], -- (r1,r2, r1*r2), 
                      let r1 = a-1, let r2 = 3*a+1,
                      let s1 = a+1, let s2 = 3*a-1,
                      isSquare (r1*r2) || isSquare (s1*s2)]

triangleArea a = if isInt areaPlus then floor areaPlus else floor areaMinus
    where areaPlus = triangleArea' a (a+1)
          areaMinus = triangleArea' a (a-1)
          triangleArea' a b = (b * sqrt (4*a*a-b*b)) / 4


