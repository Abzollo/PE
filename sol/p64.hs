
main = print $ sol
sol = length . filter odd . map (length . tail . getSeqFrac) . filter (not . isSquare) $ [2..10000]

getSeqFrac n = getSeqFrac' n (0, 1) []
getSeqFrac' n (p,q) old
    | elem (p,q) old = []
    | otherwise = a : getSeqFrac' n (newp,newq) ((p,q) : old)
    where a = floor $ (sqrt (fromIntegral n) + fromIntegral p) / (fromIntegral q)
          newp = q * a - p
          newq = div (n - newp^2) q

isSquare n = (sqrt' n)^2 == n
sqrt' = floor . sqrt . fromIntegral