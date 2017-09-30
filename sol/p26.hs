-- (0.72 secs, 812,500,696 bytes)
-- 983

import Data.Ord (comparing)
import Data.List (maximumBy)

main = print sol
sol = fst $ maximumBy (comparing snd) [(n,length (decimals n)) | n <- [3,5..999]]

decimals n = dec n [] 10
dec num old target
    | elem diff old = []
    | otherwise = c : dec num (old ++ [diff]) (diff*10)
    where c = div target num
          diff = target - num*c