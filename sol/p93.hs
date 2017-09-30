import Data.Set (fromList, toAscList)

main = print sol
sol = maximum . (`zip` abcds) . map (longestConsecNums . operate) $ abcds
    where abcds = subsets 4 [1..9]

longestConsecNums :: (Integral a) => [a] -> Int
longestConsecNums xs = succ . length . takeWhile (==1) . zipWith (-) (tail xs) $ xs

operate :: (Integral a) => [Double] -> [a]
operate = toAscList . fromList . operate'
operate' abcd@(a:bcd)
    | null bcd && isInt a && a > 0 = [floor a]
    | null bcd = []
    | otherwise = concat [operate' (x : rem) |
                            ([x1,x2], rem) <- chooseElem 2 abcd,
                            op <- [(+), (-), (*), (/)],
                            x <- [op x1 x2, op x2 x1]] 
    where isInt x = x == fromIntegral (floor x)

-- Subsets are in fst, remaining elements in snd
chooseElem 0 xs = [([] , xs)]
chooseElem _ [] = []
chooseElem k (x:xs) = map (putInFst x) (chooseElem (k - 1) xs) ++ map (putInSnd x) (chooseElem k xs)
    where putInFst x (l1,l2) = (x:l1,l2)
          putInSnd x (l1,l2) = (l1,x:l2)
subsets k xs = map fst $ chooseElem k xs

-- Show the expressions using operateShow
sol' = operateShow [1,2,5,8]
operateShow :: [Double] -> [String]
operateShow = operateShow' . map show
operateShow' abcd@(a:bcd)
    | null bcd = [a]
    | null bcd = []
    | otherwise = concat [operateShow' (expression opStr x1 x2 : rem) |
                            ([x1,x2], rem) <- chooseElem 2 abcd,
                            opStr <- ["+", "-", "*", "/"],
                            x <- [expression opStr x1 x2, expression opStr x2 x1]]
    where expression opStr x1 x2 = "(" ++ x1 ++ " " ++ opStr ++ " " ++ x2 ++ ")"
