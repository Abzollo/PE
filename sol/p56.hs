import Data.Char (digitToInt)

main = print $ sol
sol = maximum [digitalSum (n^m) | n <- [80..99], m <- [89..99]]
digitalSum = sum . map digitToInt . show
