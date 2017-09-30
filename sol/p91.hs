import Data.List (sort)

main = print sol
sol = length triangles
maxX = 50
maxY = maxX

triangles = [0 | x1 <- [1..maxX], y1 <- [0..maxY],
                 x2 <- [0..x1], y2 <- [max 1 y1 .. maxY],
                 (x1,y1) /= (x2,y2),
                 let a = x1^2 + y1^2,
                 let b = x2^2 + y2^2,
                 let c = (x2-x1)^2 + (y2-y1)^2,
                 let [x,y,z] = sort [a,b,c],
                 z == x+y]