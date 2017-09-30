import Data.Set as S

main = print sol

sol = doitbaby

doitbaby = chains2 (fromList [500003,500005..1000000]) (0,0)

chains x cset (mx,m)
    | S.null cset = (mx,m)
    | l > m = chains newx newset (x,l)
    | otherwise = chains newx newset (mx,m)
    where c = collatz x
          l = length c
          (newx, newset) = deleteFindMax $ S.difference cset (fromList c)
chains2 cset (mx,m)
    | S.null cset = (mx,m)
    | l > m = chains2 newset (x,l)
    | otherwise = chains2 newset (mx,m)
    where x = S.findMin cset
          c = collatz x
          l = length c
          newset = S.difference cset (fromList c)

collatz 1 = [1]
collatz n  
    | even n =  n:collatz (n `div` 2)  
    | odd n  =  n:collatz (n*3 + 1)
