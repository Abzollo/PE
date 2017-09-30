import qualified Data.Set as S

main = print $ 10^5 * sol + 2 * sol
sol = head [a | a <- [9499,9498..],
                let pan = digitset [a,a*2],
                not (elem '0' pan),
                length pan == 9]

digitset numsList = S.fromList . concat $ map show numsList

