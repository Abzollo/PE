
main = print sol
sol = round . sum $ zipWith ($) (map lagrangePoly [0..degree]) (tail xs)

-- The given equation and its degree
degree = 10
equation x = sum $ map ((-x)^) [0..degree]

xs = map fromIntegral [1..degree+1]  -- xi of sample points (1 to degree+1)
datapoints = zip xs (map equation xs)  -- (xi, yi), degree+1 sample points

-- Lagrange interpolating polynomial of degree n, sampling first n+1 points (= OP(n+1, x) as in PE)
lagrangePoly n x = sum $ map (lagrangePolyAtj n x) (take (n+1) datapoints)
-- jth term of Lagrange interpolating polynomial of degree n
lagrangePolyAtj n x (xj,yj) = yj * product [(x-xk)/(xj-xk) | xk <- take (n+1) xs, xk /= xj]

-- For each Lagrange interpolating polynomial of degree: 0 to degree, show the sequence p(x_n)
-- allSequences = zipWith map (map lagrangePoly [0..degree]) (repeat xs)