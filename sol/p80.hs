import Data.List ((\\))
main = print sol
sol = sum . map (sum . sqrtDigits 1000) $ [1..100] \\ squares
squares = map (^2) [1..10]

-- Works only for 2-digit numbers or less
sqrtDigits numDigits n = take numDigits (sqrtDigits' n 0)
sqrtDigits' n p = nextDigit : sqrtDigits' (100 * (n - nextDigit*(20*p+nextDigit))) (10*p + nextDigit)
    where nextDigit = last . takeWhile (\d -> d * (20*p + d) <= n) $ [0..9]