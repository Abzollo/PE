import qualified Data.Char as C

main = print sol
sol = sum goodguys

factorial a = product [1..a]

goodguys = [a | a <- [10..10^5], a == sum (map (factorial . C.digitToInt) $ show a)]