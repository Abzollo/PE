-- 8*num+1 is perfect square == num is triangle
import Data.Char (ord)

main = do
        wordsStr <- readFile "Desktop/p042_words.txt"
        let wordsList = "[" ++ wordsStr ++ "]"
            words = read wordsList
            sol = length . filter triWord $ words
        print sol
            
triWord w = issquare (1 + 8 * wordVal w)
wordVal = sum . map (\x -> ord(x) - 64)
issquare n = (floor . sqrt . fromIntegral $ n)^2 == n