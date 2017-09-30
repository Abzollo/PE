import qualified Data.Char as C

main = print sol
sol = round . product $ map (\(a,b) -> b/a) goodguys

goodguys = [(aa,bb) | a <- [11..99],
                    b <- [11..99],
                    a /= b && a < b,
                    not (mod a 10 == 0 && mod b 10 == 0),
                    let aa = realToFrac a,
                    let bb = realToFrac b,
                    checkFactors aa bb]
                    
digits number = map (fromIntegral . C.digitToInt) $ show number


checkFactors a b
    | num1 == den1 = False
    | num1 == den2 = num2/den1 == frac
    | num2 == den1 = num1/den2 == frac
    | num2 == den2 = num1/den1 == frac
    | otherwise = False
    where num1:num2:_ = digits a
          den1:den2:_ = digits b
          frac = a/b