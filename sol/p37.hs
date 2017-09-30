import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)

main = print $ sum sol
sol = take 11 [p | p <- drop 4 primes, truncprime p]

truncprime p = truncprime' p 10

truncprime' p t
    | leftp <= 0 = True
    | otherwise = isPrime leftp && isPrime rightp && truncprime' p (t*10)
    where (leftp, rightp) = quotRem p t