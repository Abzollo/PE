import qualified Data.List as L

main = print sol1

sol1 = head factorsList
sol2 = foldr1 lcm [1..20]

factorsOf list num = all (==0) $ map (mod num) list
factorsOf' list num = foldl (\acc x -> if not . (==0) . (mod num) $ x then False else acc) True list
factorsList = [x | x <- [20,40..], [19,18..11] `factorsOf'` x]