import Math.NumberTheory.Primes.Factorisation (factorise)

main = print $ head sol
sol = [n | n <- [1..], all (>=4) . map (length . factorise) $ [n,n+1,n+2,n+3]]