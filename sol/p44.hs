-- 8*num+1 is perfect square == num is triangle
-- 24*num+1 is perfect square == num is pentagon
-- (7042750,1560090)
--
-- real	0m0.287s
-- user	0m0.273s
-- sys	0m0.006s

import Data.List (tails)

main = print sol
sol = head . filter (\(a,b) -> isPent (a+b)) $ candidates 3000
candidates many = concat . pentOp . take many $ pents


pentOp = map pentOp' . filter ((>2) . length) . tails . reverse
pentOp' (a:b:t) = pentOp'' a (a-b) (b:t)
pentOp'' bigPent maxDiff (b:t)
    | b < maxDiff = []
    | isPent p = (bigPent,b) : pentOp'' bigPent maxDiff t
    | otherwise = pentOp'' bigPent maxDiff t
    where p = bigPent - b
pents = [div (3*n^2-n) 2 | n <-[1..]]
isPent p = let num = 1 + 24 * p
             in if issquare num
                  then mod (1 + sqrt' num) 6 == 0
                  else False
issquare n = (sqrt' n)^2 == n
sqrt' = floor . sqrt . fromIntegral