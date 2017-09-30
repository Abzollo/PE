main = print sol
sol = pay coins 200

coins = reverse [1,2,5,10,20,50,100,200]
pay mycoins amount = pay' (filter (<= amount) mycoins) amount
pay' [] _ = 1
pay' [1] _ = 1
pay' (c:cs) amt = combs + pay' cs amt
                where combs = sum $ map (pay cs) (subcoins amt c)
                      subcoins amt c = map ((amt-) . (*c)) [1..(div amt c)]