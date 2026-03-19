

-- Task 2
-- Coprime Pairs. Write a function coprimePairs :: [Int] -> [(Int, Int)] that takes a list of positive integers
-- and returns all unique pairs (x, y) (with x < y) for which gcd x y == 1. You may use Haskell's built-in gcd

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x:xs) = [(min x y, max x y) | y <- xs, gcd x y == 1, x /= y] ++ coprimePairs xs
