import Data.Sequence (Seq)
import Data.Foldable (Foldable(toList))
data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
   deriving Show



-- Task 1
-- Write a Functor instance for Sequence:

instance Functor Sequence where
   fmap f Empty        = Empty
   fmap f (Single a)   = Single(f a)
   fmap f (Append h t) = Append (fmap f h) (fmap f t)


-- Task 2
-- Write a Foldable instance for Sequence by implementing foldMap:


-- foldMap :: Monoid m => (a -> m) -> Sequence a -> m
instance Foldable Sequence where
   foldMap f Empty        =  mempty 
   foldMap f (Single a)   = f a
   foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList a = toList a


seqLength :: Sequence a -> Int
seqLength a = length a 



-- Task 3
instance Semigroup (Sequence a) where
   (<>) = Append 

instance Monoid (Sequence a) where
    mempty = Empty



-- Task 4
-- Tail Recursion and Sequence Search
-- Write a function
-- tailElem :: Eq a => a -> Sequence a -> Bool
-- that searches for an element in a Sequence using tail recursion with an explicit stack 
-- (a list of Sequence a values) to manage subsequences still to be inspected.


tailElem :: Eq a => a -> Sequence a -> Bool
tailElem elem seq = go [seq]
   where
      go []                  = False
      go (Empty : rest)      = go rest
      go (Single a : rest)   = (a == elem) || go rest
      go (Append l r : rest) = go (l: r: rest)


-- For tests
s0 = Empty
s1 = Single 1
s2 = Single 2
s3 = Append s0 s1
s4 = Append s3 s2



-- Task 5
-- Tail Recursion and Sequence Flatten
-- Write a tail-recursive function
-- tailToList :: Sequence a -> [a]
-- that converts a Sequence a to a list [a] in left-to-right order.

tailToList :: Sequence a -> [a]
tailToList seq = go [] [seq]
   where 
      go !acc (Empty : rest) = go acc rest
      go !acc (Single a : rest) = go (a:acc) rest
      go !acc (Append l r : rest) = go acc (l:r:rest)
      go !acc [] = reverse acc


-- Task 6
-- Tail Recursion and Reverse Polish Notation
-- A Reverse Polish Notation (RPN) expression is a sequence of tokens:
-- data Token = TNum Int | TAdd | TSub | TMul | TDiv
-- Evaluation uses a stack: numbers are pushed; operators pop two values, apply the operation, and push the result back.
-- Write a tail-recursive function
-- tailRPN :: [Token] -> Maybe Int
-- that processes the token list using a list as the operand stack accumulator.
-- Return Nothing for malformed expressions (too few operands, tokens remaining after the final result) or division by zero.

data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go [] tokens
   where
      go stack          (TNum a : tRest) = go (a : stack) tRest
      go (a : b : rest) (TAdd : tRest) = go ((b+a):rest) tRest 
      go (a : b : rest) (TMul : tRest) = go ((b*a):rest) tRest
      go (a : b : rest) (TSub : tRest) = go ((b-a):rest) tRest
      go (0 : b : rest) (TDiv : tRest) = Nothing
      go (a : b : rest) (TDiv : tRest) = go ((b `div` a):rest) tRest
      go [a] [] = Just a 
      go _ _ = Nothing
      
      
      
-- Task 7
-- Expressing functions via foldr and foldl
-- Without using explicit recursion, implement the following functions using foldr and/or foldl:
-- (a) myReverse :: [a] -> [a] — reverses a list. Use foldl.

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) [] 


-- (b) myTakeWhile :: (a -> Bool) -> [a] -> [a] — returns the longest prefix of elements 
-- satisfying the predicate (e.g. myTakeWhile even [2,4,3,6] = [2,4]). Use foldr.

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile comp = foldr(\ x acc -> if comp x then x:acc else []) [] 


-- (c) decimal :: [Int] -> Int — interprets a list of digits as a decimal number, e.g. decimal [1,2,3] = 123.

decimal :: [Int] -> Int
decimal = foldl (\ acc x -> acc*10 + x) 0


-- Task 8
-- Run-length encoding via folds
-- Run-length encoding compresses a list by replacing consecutive runs of the same element with a pair of the element and its count.
--
-- (a) Implement encode :: Eq a => [a] -> [(a, Int)] using foldr. For example:

encode :: Eq a => [a] -> [(a, Int)]
encode = foldr f []
   where 
      f x ((c, n) : rest) = if x == c then (c,n+1):rest else (x,1):(c,n):rest
      f x [] = [(x,1)] 



