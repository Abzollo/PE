import Math.NumberTheory.Primes (factorise, primes)
import Control.Arrow (first)
import Data.List (sort, find)
import qualified Data.Set as Set

-- Runs in ~20s
main = print sol
sol = sum . Set.fromList . take (maxK-1) $ minimalProductSums
maxK = 12000

minimalProductSums = findMinPS 2 2 primes Set.empty

findMinPS k minPS pAll@(p:pRem) history
    | minPS == p = findMinPS k (minPS+1) pRem history
    | Set.null setPS = findMinPS k (minPS+1) pAll history'
    | otherwise = fst (Set.findMin setPS) : findMinPS (k+1) minPS pAll history'
    where combs = tail $ multiplicativePartitions minPS
          good = Set.fromList . map (\l -> (product l, l)) . filter (goodPS k) $ combs
          (setPS, history') = Set.partition (isPS k . snd) (Set.union history good)

isPS k list = product list == k - toInteger (length list) + sum list
goodPS k list = product list >= k - toInteger (length list) + sum list

-- Copied code from the internet for finding multiplicative partition of n (by Daniel Fischer)
multiplicativePartitions :: Integer -> [[Integer]]
multiplicativePartitions n
    | n < 1     = []
    | n == 1    = [[]]
    | otherwise = map ((>>= uncurry (flip replicate)) . sort) . pfPartitions $ factorise n

additivePartitions :: Int -> [[(Int,Int)]]
additivePartitions 0 = [[]]
additivePartitions n
    | n < 0     = []
    | otherwise = aParts n n
      where
        aParts :: Int -> Int -> [[(Int,Int)]]
        aParts 0 _ = [[]]
        aParts 1 m = [[(1,m)]]
        aParts k m = withK ++ aParts (k-1) m
          where
            withK = do
                let q = m `quot` k
                j <- [q,q-1 .. 1]
                [(k,j):prt | let r = m - j*k, prt <- aParts (min (k-1) r) r]

countedPartitions :: Int -> Int -> [[(Int,Int)]]
countedPartitions 0     count = [[(0,count)]]
countedPartitions quant count = cbParts quant quant count
  where
    prep _ 0 = id
    prep m j = ((m,j):)
    cbParts :: Int -> Int -> Int -> [[(Int,Int)]]
    cbParts q 0 c
        | q == 0    = if c == 0 then [[]] else [[(0,c)]]
        | otherwise = error "Oops"
    cbParts q 1 c
        | c < q     = []        -- should never happen
        | c == q    = [[(1,c)]]
        | otherwise = [[(1,q),(0,c-q)]]
    cbParts q m c = do
        let lo = max 0 $ q - c*(m-1)
            hi = q `quot` m
        j <- [lo .. hi]
        let r = q - j*m
            m' = min (m-1) r
        map (prep m j) $ cbParts r m' (c-j)

primePowerPartitions :: Integer -> Int -> [[(Integer,Int)]]
primePowerPartitions p e = map (map (first (p^))) $ additivePartitions e

distOne :: Integer -> Int -> Integer -> Int -> [[(Integer,Int)]]
distOne _ 0 d k = [[(d,k)]]
distOne p e d k = do
    cap <- countedPartitions e k
    return $ [(p^i*d,m) | (i,m) <- cap]

distribute :: Integer -> Int -> [(Integer,Int)] -> [[(Integer,Int)]]
distribute _ 0 xs = [xs]
distribute p e [(d,k)] = distOne p e d k
distribute p e ((d,k):dks) = do
    j <- [0 .. e]
    dps <- distOne p j d k
    ys <- distribute p (e-j) dks
    return $ dps ++ ys
distribute _ _ [] = []

pfPartitions :: [(Integer,Int)] -> [[(Integer,Int)]]
pfPartitions [] = [[]]
pfPartitions [(p,e)] = primePowerPartitions p e
pfPartitions ((p,e):pps) = do
    cop <- pfPartitions pps
    k <- [0 .. e]
    ppp <- primePowerPartitions p k
    mix <- distribute p (e-k) cop
    return (ppp ++ mix)