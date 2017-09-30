import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)

main = print $ length sol
sol = goodguys

goodguys = takeWhile (<10^6) [p | p <- primes, cycleprime p || p >=10^6]

cycleprime p = cycleprime' p (cyclep p)

cycleprime' p0 p
    | p0 == p = True
    | isPrime p = cycleprime' p0 (cyclep p)
    | otherwise = False
    
cyclep p = plast * 10^(l-1) + pinit
           where (pinit, plast) = divMod p 10
                 l = length . show $ p