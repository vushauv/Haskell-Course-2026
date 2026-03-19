

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

-- permutations :: Int -> [a] -> [[a]]
-- permutations 0 _      = []
-- permutations n []     = []
-- permutations 1 (x:xs) = [[x]] ++ permutations 1 xs 
-- permutations n (x:xs) = [[

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations k xs = [x:rest | (i, x) <- zip [0..] xs, rest <- permutations (k-1) (removeAt i xs)]
