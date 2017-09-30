-- 44680
--
-- real	0m59.619s
-- user	0m58.931s
-- sys	0m0.601s

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Math.NumberTheory.Primes (isPrime)
import Data.List (sort, permutations)

main = print sol
allDigits = "123456789"
sol = S.size . S.fromList
             . map sort
             . map (\s -> read $ "[" ++ s ++ "]" :: [Int])
             . allPandigitalPrimes
             $ allDigits

primesOfDigits = M.fromList [(l, subdigitalPrimes) |
                                n <- [1..length allDigits],
                                (l, _) <- chooseRem n allDigits,
                                let subdigitalPrimes = filter (isPrime . read) $ permutations l]

allPandigitalPrimes digits  = primesOfDigits M.! digits ++
                              [primes1 ++ "," ++ primes2 |
                               n <- [1..length digits - 1],
                               (digits1, digits2) <- chooseRem n digits,
                               primes1 <- allPandigitalPrimes digits1,
                               primes2 <- allPandigitalPrimes digits2]

chooseRem 0 xs = [([],xs)]
chooseRem _ [] = []
chooseRem k (x:xs) = map (putInFst x) (chooseRem (k-1) xs) ++ map (putInSnd x) (chooseRem k xs)
    where putInFst x (xs,ys) = (x:xs,   ys)
          putInSnd y (xs,ys) = (  xs, y:ys)