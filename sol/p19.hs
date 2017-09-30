data Weekday = Mon | Tues | Wed | Thurs | Fri | Sat | Sun
               deriving (Eq,Ord,Show,Read,Bounded,Enum)
type Day = Int
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
             deriving (Eq,Ord,Show,Read,Bounded,Enum)
type Year = Int
data Date = Date { weekday :: Weekday
                 , day :: Day
                 , month :: Month
                 , year :: Year
                 } deriving (Eq,Ord,Read)
instance Show Date where
    show (Date wd d m y) = show wd ++ " "  ++ show d ++ " of " ++ show m ++ " " ++ show y


main = print sol

sol = length . firstCent . sundaysAt1 $ future givenDate

givenDate :: Date        
givenDate = Date Mon 1 Jan 1900

future :: Date -> [Date]
future = iterate nextDate

firstCent :: [Date] -> [Date]
firstCent = takeWhile (\date -> year date < 2001) . dropWhile (\date -> year date < 1901)

sundaysAt1 :: [Date] -> [Date]
sundaysAt1 = filter (\date -> weekday date == Sun && day date == 1)

tomorrow :: Weekday -> Weekday
tomorrow Sun = Mon
tomorrow d = succ d

nextMonth :: Month -> Month
nextMonth Dec = Jan
nextMonth m = succ m

nextDate :: Date -> Date
nextDate (Date wd d m y)
    | d >= maxDay = Date (tomorrow wd) 1 (nextMonth m) (checkNewYear m y)
    | otherwise = Date (tomorrow wd) (succ d) m y
    where maxDay = lastDay m y
          checkNewYear m y = if m == Dec then (succ y) else y

lastDay :: Month -> Year -> Int
lastDay m y
    | m == Jan = 31
    | m == Feb = if leapyear y then 29 else 28
    | m == Mar = 31
    | m == Apr = 30
    | m == May = 31
    | m == Jun = 30
    | m == Jul = 31
    | m == Aug = 31
    | m == Sep = 30
    | m == Oct = 31
    | m == Nov = 30
    | m == Dec = 31

leapyear :: Year -> Bool
leapyear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False
    