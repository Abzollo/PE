
main = print (length sol `div` 2)
n = 12
sol = [(s1,s2) | k <- [2 .. div n 2],
                 (s1, l2) <- subsetsR k [1..n],
                 (s2, _ ) <- subsetsR k l2,
                 not $ (\l -> all (not . id) l || all id l) $ zipWith (>) s1 s2]

subsetsR 0 xs = [([],xs)]
subsetsR _ [] = []
subsetsR k (x:xs) = map (putInFst x) (subsetsR (k - 1) xs) ++ map (putInSnd x) (subsetsR k xs)
    where putInFst x (xs,ys) = (x:xs,   ys)
          putInSnd y (xs,ys) = (  xs, y:ys)