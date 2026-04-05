data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)


-- Write a Functor instance for Sequence:

instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
