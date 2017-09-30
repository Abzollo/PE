main = do
         names <- readFile "Desktop/p022_names.txt"
         let namesList = read ("[" ++ names ++ "]") :: [String]
             sol = sumScores 1 . quicksort $ namesList
         print sol

sumScores _ [] = 0
sumScores r (h:t) = r * (sum $ namescores h) + sumScores (r+1) t
namescores =  map ((flip mod) 64 . fromEnum)

quicksort [] = []
quicksort (x:xs) = let lt = quicksort [a | a <- xs, a <= x]
                       gt = quicksort [a | a <- xs, a >= x]
                   in lt ++ [x] ++ gt