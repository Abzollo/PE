import Math.NumberTheory.Primes.Factorisation

main = print . sum . map (sieveTotient (totientSieve maxD)) $ [2..maxD]
maxD = 10^6