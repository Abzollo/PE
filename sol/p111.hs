import Data.Char (intToDigit)
import Math.NumberTheory.Primes (isPrime)

main = print sol

sol = sum $ map (sum . (findMaxRDP maxN)) [0..9]
maxN = 10

findMaxRDP n d = findMaxRDP' n n d
findMaxRDP' n m d
    | null rdp = findMaxRDP' n (m-1) d
    | otherwise = rdp
    where rdp = repDigitPrime n m d

repDigitPrime n m d = filter isPrime . map read . filter ((/='0') . head) $ repDigitPrime' n m (intToDigit d)
repDigitPrime' n m d
    | n < m = []
    | n == 0 = [[]]
    | otherwise = map (d:) (repDigitPrime' (n-1) (m-1) d)
                      ++ concat [map (other:) (repDigitPrime' (n-1) m d)
                                    | other <- "1234567890", other /= d]