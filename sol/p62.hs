import Data.List (sort)

main = mapM_ print ["Numbers: " ++ show sol, "Solution: " ++ (show . (^3) . head $ sol)]
sol = take 5 . filter ((==finalist) . sort . show . (^3)) $ [1..]
finalist = head . findDup 5 $ cand
findDup _ [] = []
findDup t (n:ns) = if count n ns == (t-1) then n : findDup t ns else findDup t ns
cand = [sort $ show (n^3) | n <- [1..10000]]
count n = length . filter (==n)