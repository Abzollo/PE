import Control.Applicative (liftA2)
import Data.List (nub, sort)

------- Poker types -------
data PokerHand = HighCard | OnePair | TwoPairs | ThreeKind | Straight | Flush | FullHouse | FourKind | StraightFlush | RoyalFlush
             deriving (Show,Read,Eq,Ord)
data Hand = Hand { card1 :: Card
                 , card2 :: Card
                 , card3 :: Card
                 , card4 :: Card
                 , card5 :: Card
                 }
instance Show Hand where
    show = showHand . sortHand
instance Eq Hand where
    hand1 == hand2 = compareHands hand1 hand2 == EQ
instance Ord Hand where
    compare hand1 hand2 = compareHands hand1 hand2

data Card = Card { rank :: CardRank
                 , suit :: CardSuit }
instance Show Card where
    show = showCard
instance Eq Card where
    (Card r1 _) == (Card r2 _) = r1 == r2
instance Ord Card where
    compare (Card r1 _) (Card r2 _) = compare r1 r2

data CardSuit = Hearts | Diamonds | Clubs | Spades
             deriving (Eq)
instance Show CardSuit where
    show Hearts   = "H"
    show Diamonds = "D"
    show Clubs    = "C"
    show Spades   = "S"

data CardRank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
             deriving (Eq,Ord,Bounded,Enum)
instance Show CardRank where
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "10"
    show Ten   = "T"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

    
------- Card & Hand Functions -------
showCard (Card r s) = show r ++ show s
readCard ([r,s]) = Card (readRank r) (readSuit s)

showHand (Hand c1 c2 c3 c4 c5) = unwords . map show $ [c1, c2, c3, c4, c5]
readHand handStr = sortHand $ Hand (readCard c1) (readCard c2) (readCard c3) (readCard c4) (readCard c5)
                   where [c1,c2,c3,c4,c5] = words handStr

readRank r
    | r == '2' = Two
    | r == '3' = Three
    | r == '4' = Four
    | r == '5' = Five
    | r == '6' = Six
    | r == '7' = Seven
    | r == '8' = Eight
    | r == '9' = Nine
    | r == 'T' = Ten
    | r == 'J' = Jack
    | r == 'Q' = Queen
    | r == 'K' = King
    | otherwise = Ace

readSuit s
    | s == 'H' = Hearts
    | s == 'D' = Diamonds
    | s == 'C' = Clubs
    | otherwise = Spades

-- Good enough for 5 elements
sortHand (Hand c1 c2 c3 c4 c5)
    | c1 > c2 = sortHand (Hand c2 c1 c3 c4 c5)
    | c2 > c3 = sortHand (Hand c1 c3 c2 c4 c5)
    | c3 > c4 = sortHand (Hand c1 c2 c4 c3 c5)
    | c4 > c5 = sortHand (Hand c1 c2 c3 c5 c4)
    | otherwise = sortHand' (Hand c1 c2 c3 c4 c5)
sortHand' hand = if rank c1 == Two &&
                    rank c2 == Three &&
                    rank c3 == Four &&
                    rank c4 == Five &&
                    rank c5 == Ace
                  then Hand c5 c1 c2 c3 c4
                  else Hand c1 c2 c3 c4 c5
                  where (Hand c1 c2 c3 c4 c5) = hand

getSuits (Hand c1 c2 c3 c4 c5) = map suit [c1,c2,c3,c4,c5]
getRanks (Hand c1 c2 c3 c4 c5) = map rank [c1,c2,c3,c4,c5]
getCards (Hand c1 c2 c3 c4 c5) = [c1,c2,c3,c4,c5]


------- Poker Hands Calculation -------
-- Assuming the cards are sorted (very important)
checkHand hand
    | isRoyalFlush hand    = RoyalFlush
    | isStraightFlush hand = StraightFlush
    | isFourKind hand      = FourKind
    | isFullHouse hand     = FullHouse
    | isFlush hand         = Flush
    | isStraight hand      = Straight
    | isThreeKind hand     = ThreeKind
    | isTwoPairs hand      = TwoPairs
    | isOnePair hand       = OnePair
    | otherwise            = HighCard

isRoyalFlush hand = isStraightFlush hand && (rank . card5 $ hand) == Ace
isStraightFlush = liftA2 (&&) isFlush isStraight
isFourKind = liftA2 (||) (allSame . tail) (allSame . init) . getRanks
isFullHouse hand = allSame [r1,r2,r3] && r4 == r5 || r1 == r2 && allSame [r3,r4,r5]
    where rs@(r1:r2:r3:r4:r5:[]) = getRanks hand
isFlush = allSame . getSuits
isStraight hand = rs == [Ace,Two,Three,Four,Five] ||
                  (r4 /= Ace && r5 == succ(r4) && r4 == succ(r3) && r3 == succ(r2) && r2 == succ(r1))
                  where rs@(r1:r2:r3:r4:r5:[]) = getRanks hand
isThreeKind hand = allSame [r1,r2,r3] || allSame [r2,r3,r4] || allSame [r3,r4,r5]
    where rs@(r1:r2:r3:r4:r5:[]) = getRanks hand
isTwoPairs hand = not (isThreeKind hand) && ((r1 == r2 || r2 == r3) && (r3 == r4 || r4 == r5))
    where rs@(r1:r2:r3:r4:r5:[]) = getRanks hand
isOnePair = (==4) . length . nub . getRanks

compareHands hand1 hand2
    | kind1 > kind2 = GT
    | kind1 < kind2 = LT
    | otherwise = compareSimilarHands kind1 hand1 hand2
    where kind1 = checkHand hand1
          kind2 = checkHand hand2

compareSimilarHands kind hand1 hand2
    | kind == FourKind  = compare (getFourKind  h1) (getFourKind  h2)
    | kind == FullHouse = compare (getFullHouse h1) (getFullHouse h2)
    | kind == ThreeKind = compare (getThreeKind h1) (getThreeKind h2)
    | kind == TwoPairs  = compare (getTwoPairs  h1) (getTwoPairs  h2)
    | kind == OnePair   = compare (getOnePair   h1) (getOnePair   h2)
    | otherwise = compare h1 h2
    where h1 = reverse $ getRanks hand1
          h2 = reverse $ getRanks hand2
          
getFourKind rs@(r1:r2:r3:r4:r5:[]) = if r1 == r2 then [r1, r5] else [r5,r1]
getFullHouse rs@(r1:r2:r3:r4:r5:[]) = if r1 == r3 then [r3,r5] else [r3,r1]
getThreeKind rs@(r1:r2:r3:r4:r5:[]) = [r3] ++ revSort (filter (/=r3) rs)
getTwoPairs rs@(r1:r2:r3:r4:r5:[])
    | r1 /= r2  = revSort [r2,r4] ++ [r1]
    | r3 /= r4  = revSort [r1,r4] ++ [r3]
    | otherwise = revSort [r1,r3] ++ [r5]
getOnePair rs@(r1:r2:r3:r4:r5:[])
    | r1 == r2  = [r1] ++ revSort [r3,r4,r5]
    | r2 == r3  = [r2] ++ revSort [r1,r4,r5]
    | r3 == r4  = [r3] ++ revSort [r1,r2,r5]
    | otherwise = [r4] ++ revSort [r1,r2,r3]


------- Useful Functions -------
allSame x = and $ map (== head x) (tail x)
revSort = reverse . sort


------- Project Euler -------
main = do
        pokertxt <- readFile "Desktop/p054_poker.txt"
        print . player1Wins . lines $ pokertxt

player1Wins = length . filter ((==GT) . pokergame)
pokergame cards = compare (readHand . unwords $ h1) (readHand . unwords $ h2)
              where (h1, h2) = splitAt 5 . words $ cards
