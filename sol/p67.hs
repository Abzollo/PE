main = p67

p67 :: IO ()
p67 = do
        t <- readFile "Desktop/p067_triangle.txt"
        let tri =  toTri t
            maxpath = maximum $ findMaxTotal tri
        putStrLn (show maxpath)

toTri :: String -> [[Int]]
toTri = map (map read . words) . lines

findMaxTotal :: [[Int]] -> [Int]
findMaxTotal tree = foldl1 addL1 tree

addL1 :: [Int] -> [Int] -> [Int]
addL1 (h:rem) (n:rest) = (n+h):addL (h:rem) rest

addL :: [Int] -> [Int] -> [Int]
addL [h] [n] = [h+n]
addL (h:rem) (n:rest) = max (n+h) (n+head rem) : addL rem rest