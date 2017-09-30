import Data.Array.IArray

main = print sol
sol = blocks maxN
maxN = 50

blocksTable :: Array Int Integer
blocksTable = listArray (-3,maxN) [blocks n | n <- [-3..maxN]]

blocks n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = blocksTable ! (n-1) +
                  blocksTable ! (n-2) +
                  blocksTable ! (n-3) +
                  blocksTable ! (n-4)