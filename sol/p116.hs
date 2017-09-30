import Data.Array.IArray

main = print sol
sol = sum . map pred . map (flip blocks maxN) $ [2,3,4]
maxN = 50

blocksTable :: Array (Int,Int) Integer
blocksTable = listArray ((2,-1), (4,maxN)) [blocks k n | k <- [2,3,4], n <- [-1..maxN]]

blocks k n
    | n < k = 1
    | otherwise = blocksTable ! (k,n-1) + blocksTable ! (k,n-k)