import Math.NumberTheory.Primes.Testing (isPrime)

main = print sol
sol = spiralPercent 0.1

spiralPrimes s = length . filter (isPrime . (+1) . ((s-1)*)) $ [s-2,s-1,s]
spiralPercent p = spiralPercent' p 3 0
spiralPercent' p s c
    | ratio < p = s
    | otherwise = spiralPercent' p (s+2) newc
    where newc = c + spiralPrimes s
          n = div (s-1) 2
          ratio = (fromIntegral newc) / (fromIntegral (4 * n + 1))
