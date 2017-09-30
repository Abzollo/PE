import Data.List (nub)
main = print sol

sol = maximum solSet

y2 = 10  -- One of the outer elements should be 10 because of 16-digit string requirement
ys = [(y3,y4,y5) | y3 <- [1..9],
                   y4 <- [1..9],
                   y5 <- [1..9],
                   length (nub [y3,y4,y5]) == 3]

makeSet [x1,x2,x3,x4,x5,y1,_,y3,y4,y5]
    | y1 == ymin = [(y1,x1,x2), (y2,x2,x3), (y3,x3,x4), (y4, x4, x5), (y5, x5, x1)]
    | y3 == ymin = [(y3,x3,x4), (y4, x4, x5), (y5, x5, x1), (y1,x1,x2), (y2,x2,x3)]
    | y4 == ymin = [(y4, x4, x5), (y5, x5, x1), (y1,x1,x2), (y2,x2,x3), (y3,x3,x4)]
    | otherwise = [(y5, x5, x1), (y1,x1,x2), (y2,x2,x3), (y3,x3,x4), (y4, x4, x5)]
    where ymin = minimum [y1,y3,y4,y5]

solSet = [makeSet gon |
              (y3,y4,y5) <- ys,
              n <- [13..19],
              let x1 = 3*n + y2 + y4 - 55,
              let x2 = 3*n + y3 + y5 - 55,
              let x3 = 55 - 2*n - y2 - y3 - y5,
              let x4 = 3*n + y2 + y5 - 55,
              let x5 = 55 - 2*n - y2 - y4 - y5,
              let y1 = 110 - 5*n - y2 - y3 - y4 - y5,
              let gon = [x1,x2,x3,x4,x5,y1,y2,y3,y4,y5],
              length (nub gon) == 10,
              all (>0) gon]