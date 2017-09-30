import Data.List (maximumBy)
import Data.Ord (comparing)

main = readFile "p099_base_exp.txt" >>= print . solve

solve = maximumBy (comparing $ logBaseExp . snd) . zip [1..] . readBaseExps
logBaseExp (b,e) = (fromIntegral e) * log (fromIntegral b)
readBaseExps =  map (\baseExp -> read $ "(" ++ baseExp ++ ")" :: (Integer,Integer)) . lines