import Math.NumberTheory.Primes.Testing (isPrime)

main = print $ sol
sol = head notGoldbachs
             
notGoldbachs = oddComps' 9 [7,5,3,2]
oddComps' n ps
    | isPrime n = oddComps' (n+2) (n:ps)
    | goldbach' n ps = oddComps' (n+2) ps
    | otherwise = n : oddComps' (n+2) ps
goldbach' n ps = any (\p -> isSquare $ div (n-p) 2) ps

isSquare n = (sqrt' n)^2 == n
sqrt' = floor . sqrt . fromIntegral