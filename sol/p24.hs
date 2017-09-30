-- (((((((((((1e6-9!*2)-8!*6)-7!*6)-6!*2)-5!*5)-4!*1)-3!*2)-2!*1)-1!*1)-0!*0) = 1
-- 2783915460

main = print sol
sol = concat . map show $ comb digits 1000000 (-1 + length digits) 0
digits = [0,1,2,3,4,5,6,7,8,9]

comb l dest c i
    | dest <= 1 = l
    | dest - fact*i >= 1 = comb l dest c (i+1)
    | otherwise = l !! (i-1) : comb (remove (i-1) l) (dest - fact*(i-1)) (c-1) 0
    where fact = product [1..c]
    
remove 0 (_:xs) = xs
remove i (x:xs) = x:remove (i-1) xs