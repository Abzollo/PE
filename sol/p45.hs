
main = print sol
sol = last . take 3 . filter (\n -> isPent n && isTri n) $ hexs

hexs = [2 * n^2 - n | n <-[1..]]
isTri t = isSquare (1 + 8 * t)
isPent p = let num = 1 + 24 * p
             in if isSquare num
                  then mod (1 + sqrt' num) 6 == 0
                  else False
isSquare n = (sqrt' n)^2 == n
sqrt' = floor . sqrt . fromIntegral