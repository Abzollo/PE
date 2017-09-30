import Data.Array.IArray

main = print sol
sol = blocks maxN
maxN = 50
blockSize = 3

blocksTable :: Array Int Integer
blocksTable = listArray (-1,maxN) [blocks n | n <- [-1..maxN]]

blocks n
    | n < blockSize = 1
    | otherwise = blocksTable ! (n-1) + sum (map (blocksTable !) [-1..n-(blockSize+1)])