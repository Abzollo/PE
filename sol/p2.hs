main = print sol

sol = sum $ filter even (fib [2,1])
fib (a:b:l) = if a+b < 4000000 then fib (a+b:a:b:l) else a:b:l