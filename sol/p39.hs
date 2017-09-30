import Data.List (maximumBy)
import Data.Ord (comparing)

main = print $ fst sol
sol = numsol 1000

rightTriangles p = [(a,b,p-a-b) | a <- [1..(p-3)/3], b <- [1..(p-a)/2], a^2 + b^2 == (p-a-b)^2]
numsol up = maximumBy (comparing snd) . map (\x -> (x, length $ rightTriangles x)) $ [1..up]