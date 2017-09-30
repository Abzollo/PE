
main = print sol

sol = numOfIncr maxN + numOfDecr maxN - numOfRept maxN 
maxN = 100

-- Number of increasing numbers under 10^k (excluding 0)
numOfIncr k = (k+9) `choose` k - 1

-- Number of decreasing numbers under 10^k
-- sum of (i+9) `choose` 9 - 1, from 1 to k, which is...
numOfDecr k = (k+10) `choose` 10 - k - 1

-- Number of repeated-digit numbers (i.e. increasing AND decreasing numbers) under 10^k
numOfRept k = 9*k

-- The choose function
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k