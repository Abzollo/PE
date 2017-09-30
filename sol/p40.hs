
main = print $ sol
sol = product $ map (champDigit . (10^)) [0..6]

champDigit d = champDigit' d 1
champDigit' d l
    | div d l >= t = champDigit' (d-t*l) (l+1)
    | otherwise = getDigit n (l-1 - mod adjusted_d l)
    where t = 9 * 10^(l-1)
          nines = 10^(l-1)-1
          adjusted_d = d + l-1
          n = nines + div adjusted_d l
          getDigit num d = mod (div num (10^d)) 10