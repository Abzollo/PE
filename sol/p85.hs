main = print sol

target = 5*10^18
-- Complicated but fast solution
sol =  (\(_,h,w)->(h,w)) $ minimum closestGrids
closestGrids = map solve $ [1..sqrtsqrt (16*target)]
    where sqrtsqrt = floor . sqrt . sqrt . fromIntegral
          f x = (-1 + sqrt (1 + (fromIntegral (16*target)) / (fromIntegral (x*x+x)))) / 2
          rects h w = (h*h+h)*(w*w+w) `div` 4
          solve h = let w = round (f h)
                    in (abs (target - rects h w), h, w)

-- Simple but fast-enough for 2,000,000
sol' = minimum allGrids
allGrids = [(t,h,w) | h <-[1..200000], w <- [h..200000], let rects = (h*h+h)*(w*w+w) `div` 4, let t = abs (target - rects)]