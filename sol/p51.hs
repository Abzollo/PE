import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List (nub)
import Data.Char (chr, digitToInt)

main = print $ head (head sol)
sol = filter (all (>100000)) $ finalists 8
finalists f = nub . filter (not . null) . concat . map (\x -> map (inFamily f x) . dupDigits $ x) $ candidates
candidates =  takeWhile (<1000000) . dropWhile (<100000) $ primes

inFamily f num m = let fam = family num m in if f <= (length fam) then fam else []
family num m = filter (isPrime) . map (replace num m) $ [0..9]

replace num m n = read $ replace' (show num) (chr $ m + 48) (chr $ n + 48)
replace' num m n = map swap num
                  where swap c = if c == m then n else c

dupDigits = dupDigits' . show
dupDigits' [] = []
dupDigits' (a:t) = if elem a t then (digitToInt a) : dupDigits' t else dupDigits' t