
main = print $ expected 30
expected 1 = 0
expected n = (2^(n-1) - 1) / (fromIntegral n) + expected (n-1)