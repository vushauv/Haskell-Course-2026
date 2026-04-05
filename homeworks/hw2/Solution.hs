data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
   deriving Show



-- Task 1
-- Write a Functor instance for Sequence:

instance Functor Sequence where
   fmap f Empty        = Empty
   fmap f (Single a)   = Single(f a)
   fmap f (Append h t) = Append (fmap f h) (fmap f t)
