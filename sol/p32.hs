import qualified Data.Set as S

main = print sol
sol = sum . S.fromList $ map (\(a,b) -> a*b) (firstrun ++ secondrun)

pandigital digits = not (elem '0' digits) && length digits == 9
digitset numsList = S.fromList . concat $ map show numsList

firstrun = [(a,b) | a <- [1..9],
                      b <- [1234..9876],
                      length (digitset [b]) == 4,
                      let c = a*b,
                      c < 10000,
                      let digits = digitset [a,b,c],
                      pandigital digits]

secondrun = [(a,b) | a <- [12..98],
                      b <- [123..987],
                      length (digitset [b]) == 3,
                      let c = a*b,
                      c < 10000,
                      let digits = digitset [a,b,c],
                      pandigital digits]