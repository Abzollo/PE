

main = mapM_ print ["Numbers: " ++ show sol, "Solution: " ++ (show . length $ sol)]
sol = concat $ map cand [1..9]
cand n = [(n,p) | p <- [1..30], p == (length . show $ n^p)]
