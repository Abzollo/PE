import Data.List
import qualified Data.Set as Set

main = print sol
sol = Set.size . Set.fromList . filter squaresInCubes $ [sort [c1,c2] | c1 <- allCubes, c2 <- allCubes]

allCubes = subsets 6 [0..9]
squares = [01, 04, 09, 16, 25, 36, 49, 64, 81]
squaresDigits = map (`divMod` 10) squares

subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs

-- Functions for checking condition
squaresInCubes [cube1, cube2] = all (digitsInCubes cube1 cube2) squaresDigits
digitsInCubes cube1 cube2 (d1,d2)
    | is6or9 d1 && is6or9 d2 = cond d1 d2 || cond (change d1) d2 || cond d1 (change d2) || cond (change d1) (change d2)
    | is6or9 d1 = cond d1 d2 || cond (change d1) d2
    | is6or9 d2 = cond d1 d2 || cond d1 (change d2)
    | otherwise = cond d1 d2
    where cond d1 d2 = elem d1 cube1 && elem d2 cube2 || elem d2 cube1 && elem d1 cube2
          is6or9 d = d == 6 || d == 9
          change d = if d == 6 then 9 else 6