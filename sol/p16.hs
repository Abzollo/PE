import Data.Char
main = print sol
sol = sum (map (digitToInt) (show (2^1000)))