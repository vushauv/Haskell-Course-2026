

-- Task 2
-- Coprime Pairs. Write a function coprimePairs :: [Int] -> [(Int, Int)] that takes a list of positive integers
-- and returns all unique pairs (x, y) (with x < y) for which gcd x y == 1. You may use Haskell's built-in gcd

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x:xs) = [(min x y, max x y) | y <- xs, gcd x y == 1, x /= y] ++ coprimePairs xs




-- Task 5
-- Permutations. Write a function permutations :: Int -> [a] -> [[a]] that generates all k-element permutations
-- (ordered selections without repetition) from a given list.
-- For example, for k = 2 and list [1,2,3] the result should be [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations k xs = [x:rest | (i, x) <- zip [0..] xs, rest <- permutations (k-1) (removeAt i xs)]


-- Task 3
-- Sieve of Eratosthenes The Sieve of Eratosthenes is an ancient algorithm for finding all primes up to a given limit.
-- It works as follows: starting from the list [2..n], take the first element p — it must be prime — then remove all multiples of p from the rest of the list and repeat.

-- Implement this as a recursive function sieve :: [Int] -> [Int], where each recursive step uses a list comprehension to filter out multiples of the head. Then define:
-- primesTo :: Int -> [Int]
-- primesTo n = sieve [2..n]

-- Finally, use primesTo to define isPrime :: Int -> Bool that checks whether a given positive integer is prime.

sieve :: [Int] -> [Int]
sieve []   = []
sieve (x:xs) = [x] ++ sieve [xi | xi <- xs, xi `mod` x /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime x
   | x < 2 = False
   | otherwise = last (primesTo x) == x


-- Task 1
-- Goldbach Pairs. Write a function goldbachPairs :: Int -> [(Int, Int)] that, given an even integer n ≥ 4, returns all pairs (p, q) satisfying:
-- p and q are both prime numbers
-- p + q == n
-- p ≤ q
-- Use a list comprehension to generate the result. Define a helper isPrime :: Int -> Bool using Exercise 3.


goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
   | n < 4 = []
   | odd n = []
--   | otherwise = [(p,q) | p <- primes, q <- primes, p <= q, p + q == n]
--   where primes = primesTo n
   | otherwise = [(p,q) | p <- [2..n], q <- [2..n], isPrime p, isPrime q , p <= q, p + q == n]



-- Task 7
-- Integer Power with Bang Patterns.
-- Write a recursive function power :: Int -> Int -> Int that computes power b e = b ^ e using an accumulator.
-- Use bang patterns on the accumulator to ensure strict evaluation.

power :: Int -> Int -> Int
power b e
   | e < 0     = 0
   | otherwise = power' 1 e
   where
      power' !acc 0 = acc
      power' !acc e = power' (acc * b) (e-1)


-- Task 8
-- Running Maximum: seq vs. Bang Patterns Implement two versions of a function listMax :: [Int] -> Int 
-- that returns the maximum element of a non-empty list using a helper with an accumulator:
--
-- The first version uses seq to force evaluation of the accumulator in the helper function.
-- The second version uses bang patterns on the accumulator argument of the helper function.

listMax :: [Int] -> Int
listMax (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (x:xs) = let newAcc = max acc x
                    in seq newAcc (go newAcc xs)

listMax2 :: [Int] -> Int
listMax2 (x:xs) = go x xs
   where
      go !acc [] = acc
      go !acc (x:xs) = go (max acc x) xs


-- task 10
-- Strict Accumulation and Space Leaks Computing the mean of a list requires knowing both the sum and the length.
-- Write a function mean :: [Double] -> Double
-- using a tail-recursive helper. Do not use any library functions for the recursion.

-- (a) Write a first version with no strictness annotations.

mean1 :: [Double] -> Double
mean1 [] = 0 -- conceptually to consider
mean1 (x:xs) = go x 1 xs
   where
      go sumAcc lenAcc []     = sumAcc / lenAcc
      go sumAcc lenAcc (x:xs) = go (sumAcc+x) (lenAcc+1) xs 

-- (b) Fix the space leak using bang patterns. Is a bang pattern on the pair itself sufficient, or do the components also need to be forced individually?

mean2 :: [Double] -> Double
mean2 [] = 0 -- conceptually to consider
mean2 (x:xs) = go x 1 xs
   where
      go !sumAcc !lenAcc []     = sumAcc / lenAcc
      go !sumAcc !lenAcc (x:xs) = go (sumAcc+x) (lenAcc+1) xs 

-- (c) Generalise your strict solution to compute both the mean and the variance σ² = (Σxᵢ²)/n − μ² in a single pass. Apply bang patterns appropriately to all three components.

mean3 :: [Double] -> (Double, Double)
mean3 (x:xs) = go x (x^2) 1 xs
  where
    go !sumAcc !sumSqAcc !lenAcc [] = 
        let mean = sumAcc / lenAcc
        in (mean, sumSqAcc / lenAcc - mean^2)
    go !sumAcc !sumSqAcc !lenAcc (x:xs) =
        go (sumAcc + x) (sumSqAcc + x^2) (lenAcc + 1) xs


-- Task 4
-- Matrix Multiplication Represent a matrix as [[Int]] (a list of rows). Write
-- matMul :: [[Int]] -> [[Int]] -> [[Int]]
-- using nested list comprehensions. If the first matrix has dimensions m × p and the second p × n, then the entry at row i, column j of the product is:
--
-- sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ]
-- The outer comprehension should range over row indices i and column indices j.

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = [[sum [a !! i !! k * b !! k !! j | k <- [0..p-1]] | j <- [0..n-1]] | i <- [0..m-1]]
  where
    m = length a
    p = length b
    n = length (head b)


-- Task 6
-- Hamming Numbers A Hamming number is a positive integer whose only prime factors are 2, 3, and 5 — numbers of the form 
-- 2^a × 3^b × 5^c with a, b, c ≥ 0. The sequence begins: 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, …
-- (a) Write a helper
-- merge :: Ord a => [a] -> [a] -> [a]
-- that merges two sorted (potentially infinite) lists into one sorted list, eliminating duplicates.

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y    = x : merge xs (y:ys)
  | x == y   = x : merge xs ys        
  | otherwise = y : merge (x:xs) ys


--(b) Using merge, define the infinite list
-- hamming :: [Integer]
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming)
                    (merge (map (*3) hamming)
                           (map (*5) hamming))


