
main = print sol
sol = snd . head . dropWhile (\(a,b) -> div (100*a) b < target) . zip [1..] . filter bouncy $ [1..]
target = 99

bouncy n = let s = show n in not (isDecr s || isIncr s)

isIncr [_] = True
isIncr (a:b:rest) = a <= b && isIncr (b:rest)

isDecr [_] = True
isDecr (a:b:rest) = a >= b && isDecr (b:rest)