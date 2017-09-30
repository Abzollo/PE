-- (255,[20,31,38,39,40,42,45])

import qualified Data.Set as Set

main = print sol
sol = minimum findThem
test = [11, 18, 19, 20, 22, 25]

findThem = [(sum l,l) | let a0 = 16, a1 <- [a0+1..a0+10],
                        a2 <- [a1+1..a1+15], a3 <- [a2+1..a2+10],
                        a4 <- [a3+1..a3+5], a5 <- [a4+1..a4+5],
                        a6 <- [a5+1..a5+10], a7 <- [a6+1..50],
                        let l = [a1,a2,a3,a4,a5,a6,a7],
                        let allSubsetsSums = map (map sum) $ allSubsets l,
                        cond2 allSubsetsSums, cond1 allSubsetsSums]

cond1 = allUnique . concat
cond2 allSums = all (>0) $ zipWith (-) (map minimum $ tail allSums) (map maximum allSums)
                        
allUnique l = length l == (Set.size . Set.fromList $ l)

subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs
allSubsets l = [subsets k l | k <- [0..length l]]