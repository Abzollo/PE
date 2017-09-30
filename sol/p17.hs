main = print sol
sol = sum $ fmap wordmap [1..1000]

wordmap n
    | n < 20 = nummap n
    | n < 100 = tensmap n
    | n `mod` 100 == 0 && n < 1000 = 7 + nummap (n `div` 100) 
    | n < 1000 = 7 + hundmap n
    | n == 1000 = 11
    | otherwise = 0
    where tensmap n = num10map (10*(n `div` 10)) + nummap (n `mod` 10)
          hundmap n = nummap (n `div` 100) + 3 + wordmap (n `mod` 100)

nummap n
    | n == 1 = 3
    | n == 2 = 3
    | n == 3 = 5
    | n == 4 = 4
    | n == 5 = 4
    | n == 6 = 3
    | n == 7 = 5
    | n == 8 = 5
    | n == 9 = 4
    | n == 10 = 3
    | n == 11 = 6
    | n == 12 = 6
    | n == 13 = 8
    | n == 14 = 8
    | n == 15 = 7
    | n == 16 = 7
    | n == 17 = 9
    | n == 18 = 8
    | n == 19 = 8
    | otherwise = 0

num10map n
    | n == 20 = 6
    | n == 30 = 6
    | n == 40 = 5
    | n == 50 = 5
    | n == 60 = 5
    | n == 70 = 7
    | n == 80 = 6
    | n == 90 = 6
    | otherwise = 0