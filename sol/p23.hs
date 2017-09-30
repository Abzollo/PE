-- 4179871 (80-90 seconds) in GHCi
-- with -O
-- real	0m37.911s
-- user	0m37.508s
-- sys	0m0.329s

import qualified Data.Set as S

main = print sol

sol = let s = abundantNums 28111
          ss = S.fromList [x+y|x<-s,y<-s]
      in sum [x|x<-[1..28123], S.notMember x ss]

sumFactors :: Int -> Int
sumFactors n = let c = limit n in if c*c == n then (-c) + sumFactors' n else sumFactors' n
sumFactors' :: Int -> Int
sumFactors' n
    | even n = 1 + sum [c+(div n c) | c <- [2..(limit n)], mod n c == 0]
    | otherwise = 1 + sum [c+(div n c) | c <- [3,5..(limit n)], mod n c == 0]

limit :: Int -> Int
limit = floor . (**0.5) . fromIntegral

abundant :: Int -> Bool
abundant n = sumFactors n > n

abundantNums :: Int -> [Int]
abundantNums n = filter abundant [2..n]

