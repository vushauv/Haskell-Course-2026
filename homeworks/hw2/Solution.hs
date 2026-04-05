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






















