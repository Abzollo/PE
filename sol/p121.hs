
main = print sol
turns = 15

-- Casino profit = $1*(1-p) - $x*p > 0, so $x > (1-p)/p
-- Notice p and p' are not probabilities, just the number of ways of that event happening
sol = ceiling (fromIntegral p' / fromIntegral p)
    where p = probWin turns
          p' = product [2..turns+1] - p

-- probWin is the total number of wins when the game has n turns.
-- This is simply the number of ways we can lose less than half of the turns.
probWin n = sum [loseGame k n | k <- [0..div (n-1) 2]]
-- loseGame gives the total number of ways of losing k times in a game of n turns
-- Basically for each choice of turns in which we are gonna lose, we fix the number of
-- losing balls (-1 since one ball is winning), and then simply count the number of ways
-- to pick those balls (just their product since the choice is independent every turn),
-- and then sum these values to get the number of ways we can lose.
loseGame k n = sum . map (product . map pred) . chooseList k $ [2..n+1]

-- A choose function for lists
chooseList 0 _ = [[]]
chooseList _ [] = []
chooseList r (x:xs) = map (x:) (chooseList (r-1) xs) ++ chooseList r xs