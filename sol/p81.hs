import Data.Array.IArray
import Data.List.Split

main = do
        grid <- readFile "p081_matrix.txt"
        print (solve grid)

solve = shortestPath . toTable

toTable :: String -> Array (Int,Int) Int
toTable = listArray ((1,1),(80,80)) . concat . gridOfInts

gridOfInts :: String -> [[Int]]
gridOfInts = map (map read . splitOn ",") . lines

shortestPath :: Array (Int,Int) Int -> Int
shortestPath gridTable = sp (80,80)
    where spTable :: Array (Int,Int) Int
          spTable = listArray ((1,1),(80,80)) [sp (i,j) | i <- [1..80], j <- [1..80]]
          sp (1,1) = gridTable ! (1,1)
          sp (i,j)
              | i <= 1 = val + spTable ! (i,j-1)
              | j <= 1 = val + spTable ! (i-1,j)
              | otherwise = val + min (spTable ! (i,j-1)) (spTable ! (i-1,j) )
              where val = gridTable ! (i,j)