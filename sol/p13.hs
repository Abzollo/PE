import qualified Data.Char as C
import qualified Data.List as L

main = do
        let fileName = "Desktop/p013_num.txt"
        s <- readFile fileName
        putStrLn "How many digits, sir?"
        userN <- getLine
        if (read userN) > 14
            then do putStrLn "Too much, sir."
                    p13
            else
                let mat = readBigNums s
                    num = firstNdigits (read userN) mat
                in do putStrLn "Here you go, sir."
                      putStrLn num

readBigNums = (map (map C.digitToInt)) . L.transpose . lines
firstNdigits n m = let numnums = length $ head m
                       extraN = length $ show (9*numnums)
                   in (take n) . show . psum . reverse . (take $ n + extraN) $ m
psum [] = 0
psum (d:rest) = sum d + 10*(psum rest)