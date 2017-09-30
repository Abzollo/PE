main = do
        t <- readFile "Desktop/p018_triangle.txt"
        let tri =  toTri t
            maxpath = maximum $ findMaxTotal tri
        putStrLn (show maxpath)

toTri :: String -> [[Int]]
toTri = map (map read . words) . lines

findMaxTotal tree = foldl1 pathSums tree

pathSums (h:rem) (n:rest) = (n+h):addL (h:rem) rest

addL [h] [n] = [h+n]
addL (h:rem) (n:rest) = max (n+h) (n+head rem) : addL rem rest