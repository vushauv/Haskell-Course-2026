

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
   | otherwise = power' 1 b e   

power' :: Int -> Int -> Int -> Int
power' acc b 0 = acc
power' acc b e = power' (acc * b) b (e-1)

