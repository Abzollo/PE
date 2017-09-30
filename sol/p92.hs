import Data.Char (digitToInt)

main = print (sol + 1)
sol = length . filter (==89) . map (squareDigitSum . last . (0:) . squareDigitChain) $ [1..maxN]
maxN = 10^7

squareDigitSum = sum . map ((^2) . digitToInt) . show
squareDigitChain = takeWhile (not . (`elem` [1,89])) . iterate squareDigitSum