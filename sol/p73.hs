

main = print $ countFracsBetween (1,3) (1,2)
maxD = 12000

countFracsBetween (p1,q1) (p2,q2)
    | new_q <= maxD = 1 + (countFracsBetween (p1,q1) (new_p, new_q)) + (countFracsBetween (new_p, new_q) (p2,q2))
    | otherwise = 0
    where new_p = p1+p2
          new_q = q1+q2