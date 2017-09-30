import Math.NumberTheory.Primes.Factorisation
import Data.Array.IArray

main = print sol
sol = minimum . snd . maximum . map (\(_,chain) -> (length chain, chain)) . assocs $ amicableChainTable
sol' = filter (not . null . snd) . assocs $ amicableChainTable
maxN = 10^6

fs = factorSieve maxN
sumDivisors = sum . init . allDivisors . sieveFactor fs
allDivisors = map product . sequenceA . map (\(p,m) -> map (p^) [0..m])
sumDivisorsTable :: Array Integer Integer
sumDivisorsTable = listArray (0,maxN) $ 0 : [sumDivisors n | n <- [1..maxN]]

amicableChainTable :: Array Integer [Integer]
amicableChainTable = listArray (0,maxN) $ [] : [amicableChain n | n <- [1..maxN]]
amicableChain n
    | d < n = amicableChainTable ! d
    | otherwise = getOrderedAmicableChain n
    where d = sumDivisorsTable ! n

getOrderedAmicableChain n = getOrderedAmicableChain' n (sumDivisorsTable ! n) [n]
getOrderedAmicableChain' n d chainSoFar
    | d == 0 || d > maxN = []
    | d < n = amicableChainTable ! d
    | not (null rest) = d : reverse chain
    | otherwise = getOrderedAmicableChain' n (sumDivisorsTable ! d) (d : chainSoFar)
    where (chain, rest) = span (/=d) chainSoFar