import qualified Data.Map.Strict as Map
import Data.List (sort)

main = readFile "p098_words.txt" >>= mapM_ print . solve

solve = matchPermutations . matchWordsSqs . makeAnagramMap . readWords
readWords words =  read $ "[" ++ words ++ "]" :: [String]

makeAnagramMap words = Map.toList
                       . Map.filter ((>1) . length)
                       . Map.fromListWith (++) $
                       zip (map sort words) (map (:[]) words)

matchWordsSqs wordsAnagramMap = zip (map snd wordsAnagramMap) . map (matchWordWith squaresAnagramMap) $ wordsAnagramMap
    where l = maximum . map (length . fst) $ wordsAnagramMap
          squaresAnagramMap = makeAnagramMap . takeWhile ((<=l) . length) . map (show . (^2)) $ [1..]
          matchWordWith numAnagramMap word =  map snd . filter (match (fst word) . fst) $ numAnagramMap

-- Check if a matching between each letter and a number exists
-- (assumes word and num strings are sorted)
match (w:ws) (n:ns) 
    | null ws = null ns
    | null ns = False
    | w == head ws = n == head ns && match ws ns
    | n == head ns = False
    | otherwise = match ws ns

-- This doesn't really give the exact answer, but it's easy to see it among the few choices
matchPermutations = filter (not . null . snd) . map matchPerm
matchPerm (words@(w:ws), squaresLists) = (w:ws, filter matchAnagrams squaresLists)
    where charMap = Map.fromList . zip w
          matchAnagrams [] = False
          matchAnagrams (sq:sqs) = all ((`elem` sqs) . map ((charMap sq) Map.!)) ws || matchAnagrams sqs
          