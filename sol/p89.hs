
main = readFile "p089_roman.txt" >>= print . solve
solve = sum . map (savedChars . read) . lines
    where savedChars num = lengthOfNumeral num - lengthOfNumeral (reduceRomanNumeral num)
          lengthOfNumeral (RomanNumeral a) = length a

test = map (reduceRomanNumeral . read) $ ["MCCCCCCVI", "XIIIIIIIII", "XXXXIX"]

-- Roman Denominations
data RomanDenom = I | V | X | L | C | D | M
        deriving (Show,Read,Eq,Ord,Bounded,Enum)

readRomanDenom :: Char -> RomanDenom
readRomanDenom denom
    | denom == 'I' = I
    | denom == 'V' = V
    | denom == 'X' = X
    | denom == 'L' = L
    | denom == 'C' = C
    | denom == 'D' = D
    | otherwise =  M
    
fromRomanDenom :: RomanDenom -> Int
fromRomanDenom denom
   | denom == I = 1
   | denom == V = 5
   | denom == X = 10
   | denom == L = 50
   | denom == C = 100
   | denom == D = 500
   | denom == M = 1000
   | otherwise = 0
   
-- Roman Numerals
newtype RomanNumeral = RomanNumeral [RomanDenom]
instance Read RomanNumeral where
    readsPrec _ input = [(readRomanNumeral input, [])]
instance Show RomanNumeral where
    show (RomanNumeral num) = concat $ map show num
instance Eq RomanNumeral where
    num1 == num2 = (fromRomanNumeral num1) == (fromRomanNumeral num2)
instance Ord RomanNumeral where
    num1 `compare` num2 = (fromRomanNumeral num1) `compare` (fromRomanNumeral num2)
instance Enum RomanNumeral where
    fromEnum = fromRomanNumeral
    toEnum = toRomanNumeral
    
readRomanNumeral :: String -> RomanNumeral
readRomanNumeral = RomanNumeral . map readRomanDenom

fromRomanNumeral :: RomanNumeral -> Int
fromRomanNumeral (RomanNumeral romanNum) = fromRomanNum romanNum
fromRomanNum :: [RomanDenom] -> Int
fromRomanNum [] = 0
fromRomanNum (denom:[]) = fromRomanDenom denom
fromRomanNum (denom1:denom2:denomt)
    | denom1 >= denom2 = fromRomanDenom denom1 + fromRomanNum (denom2:denomt)
    | otherwise = (fromRomanDenom denom2 - fromRomanDenom denom1) + fromRomanNum denomt

toRomanNumeral :: Int -> RomanNumeral
toRomanNumeral n = RomanNumeral (toRomanNum n)
toRomanNum n
    | n >= 1000 = let (quot,rem) = divMod n 1000 in replicate quot M ++ toRomanNum rem
    | n >= 100 = let (quot,rem) = divMod n 100 in toRomanNum' (C,D,M) quot ++ toRomanNum rem
    | n >= 10 = let (quot,rem) = divMod n 10 in toRomanNum' (X,L,C) quot ++ toRomanNum rem
    | otherwise = toRomanNum' (I,V,X) n

toRomanNum' (one,five,ten) num
    | num <= 3 = replicate num one
    | num == 4 = [one,five]
    | num == 5 = [five]
    | num <= 8 = [five] ++ replicate (num-5) one
    | otherwise = [one,ten]

reduceRomanNumeral :: RomanNumeral -> RomanNumeral
reduceRomanNumeral = toRomanNumeral . fromRomanNumeral

