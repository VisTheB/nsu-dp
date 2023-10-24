module Solution
    ( unique
    , sort
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , primes
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where


unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:hvost)
    | x `elem` hvost = False 
    | otherwise = unique hvost




sort :: (Ord a, Num a) => a -> a-> a -> (a, a, a)
sort x y z = (minimum [x, y, z], sum [x, y, z] - maximum [x, y, z] - minimum [x, y, z], maximum [x, y, z])

-- Это решение не проходит, потому что там тройки не совсем как надо отсортированы. Поэтому я ниже вставила обычное решение, чтобы оно твои тесты прошло
-- pythagoreanTriples :: Integral a => [(a, a, a)]
-- pythagoreanTriples = [sort (k * (m*m - n*n)) (k * (2*m*n)) (k * (m*m + n*n)) |
--                         m <- [2..10],
--                         n <- [1..m- 1],
--                         m > n,
--                         odd (m - n),
--                         gcd m n == 1,
--                         k <- [1..m+2]]
-- 🤔

-- Вот обычное
pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a*a + b*b == c*c]

-- Тут то же самое
-- primitivePythagoreanTriples :: Integral a => [(a, a, a)]
-- primitivePythagoreanTriples = [sort (m*m - n*n) (2*m*n) (m*m + n*n) | m <- [2..], n <- [1..m-1], m > n, odd (m - n), gcd m n == 1]

-- Вот обычное
primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a*a + b*b == c*c, gcd a b == 1]



primes :: [Int]
primes = sieve [2..]
    where 
        sieve [] = []
        sieve (x:hvost) = x : sieve [i | i <- hvost, i `mod` x /= 0]

perfectNumbers :: Integral a => [a]
perfectNumbers = [2^(k-1) * (2^k - 1) | k <- primes, (2^k - 1) `elem` primes]




cantorPairs :: Integral a => [(a, a)]
cantorPairs = concatMap (\n -> zip [n, n-1..0] [0..n]) [0..]


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
