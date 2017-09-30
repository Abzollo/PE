import Data.Array.IArray
import Data.List (find)

main = print sol
sol = find ((>target) . blocks) [1..maxN]
maxN = 1000
target = 10^6
blockSize = 50

blocksTable :: Array Int Integer
blocksTable = listArray (-1,maxN) [blocks n | n <- [-1..maxN]]

blocks n
    | n < blockSize = 1
    | otherwise = blocksTable ! (n-1) + sum (map (blocksTable !) [-1..n-(blockSize+1)])