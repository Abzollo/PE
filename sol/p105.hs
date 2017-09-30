import qualified Data.Set as Set

main = readFile "p105_sets.txt" >>= print . solve
solve = sum . map fst
            . filter (cond1 . snd) . filter (cond2 . snd)
            . map (\l -> (sum l, allSubsetsSums l))
            . map (\nums -> read $ "[" ++ nums ++ "]" :: [Int])
            . lines

allSubsetsSums = map (map sum) . allSubsets
cond1 = allUnique . concat
cond2 allSums = all (>0) $ zipWith (-) (map minimum $ tail allSums) (map maximum allSums)
                        
allUnique l = length l == (Set.size . Set.fromList $ l)

subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs
allSubsets l = [subsets k l | k <- [0..length l]]