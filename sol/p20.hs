import Data.Char
main = print sol
sol = sum . map (digitToInt) . show . product $ [1..100]