
main = print $ sol
sol = sum $ map (\x -> length $ filter (>1000000) $ map (choose x) [0..x]) [1..100]
choose n r = product [(r+1)..n] `div` product [1..(n-r)]

--zipWith (+) [23..] [i + count n | n <- [23..100], let i = if even n then 1 else 0]
--count n = -2 * halfCount n
--Why the hell this is not working for, say, 30, but 30 + (-2 * halfCount 30) gives the correct result????????
--fullCount x = x + (-2 * halfCount x)
--halfCount n = length $ takeWhile ((< 1000000) . choose n) [r | r <- [0..(div n 2)]]
--fac n = product [1..n]
