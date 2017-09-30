
main = mapM_ print ["Numbers: " ++ show sol, "Solution: " ++ (show . sum . map fst $ sol)]
sol = head finalists

finalists = filter (\l -> cyclic (fst $ head l) (fst $ last l)) $ iterate filterCands cand1 !! 4
filterCands cand = [(n2,t2):ns  |  ns <- cand, let (n1,t1) = head ns,  (n2,t2) <- family,
                                   not . elem t2 . map snd $ ns,  cyclic n1 n2]
cand1 = [[(n2,t2),(n1,t1)] | (n1,t1) <- octagonals, (n2,t2) <- family, t1 /= t2, cyclic n1 n2]
cyclic n1 n2 = (mod n1 100) == (div n2 100)

-- Generating 4-digits polygon numbers
family = concat [triangles, squares, pentagonals, hexagonals, heptagonals, octagonals]
triangles = [(tri n, 3) | n <- [45..140]]
squares = [(sqr n, 4) | n <- [32..99]]
pentagonals = [(pnt n, 5) | n <- [26..81]]
hexagonals = [(hex n, 6) | n <- [23..70]]
heptagonals = [(hpt n, 7) | n <- [21..63]]
octagonals = [(oct n, 8) | n <- [19..58]]
tri n = div (n*(n+1)) 2
sqr n = n^2
pnt n = div (n*(3*n-1)) 2
hex n = n*(2*n-1)
hpt n = div (n*(5*n-3)) 2
oct n = n*(3*n-2)