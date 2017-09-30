import Data.List (sort)

main = readFile "p102_triangles.txt" >>= print . solve
solve = length . filter (inTriangle target)
               . map (toTriangle . (\nums -> read $ "[" ++ nums ++ "]" :: [Double]))
               . lines

target = (0,0)

toTriangle [x1,y1,x2,y2,x3,y3] = sort [(x1,y1),(x2,y2),(x3,y3)]
inTriangle (x,y) [(x1,y1),(x2,y2),(x3,y3)]
    | x < x1 || x > x3 = False
    | y2 > line13 x2 = y <= line12 x && y <= line23 x && y >= line13 x
    | y2 < line13 x2 = y >= line12 x && y >= line23 x && y <= line13 x
    | otherwise = False
    where line12 = getLineEq (x1,y1) (x2,y2)
          line23 = getLineEq (x2,y2) (x3,y3)
          line13 = getLineEq (x1,y1) (x3,y3)
          
getLineEq (x1,y1) (x2,y2) = (+b) . (*a)
    where a = (y2-y1)/(x2-x1)
          b = y1 - a*x1