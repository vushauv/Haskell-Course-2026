import qualified Data.Map as Map
import Control.Monad (foldM, guard)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.List (permutations, sort)

-- Task 1 Maze Navigation

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map.Map Pos (Map.Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = Map.lookup pos maze >>= Map.lookup dir

-- (b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start dirs = foldM (move maze) start dirs

-- (c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = fmap reverse $ foldM step [start] dirs
  where
    step acc@(pos : _) dir = do
      next <- move maze pos dir
      return (next : acc)


-- Task 2 Decoding a message

type Key = Map.Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)


-- Task 3 Seating arrangements

type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings [] _ = [[]]
seatings guests conflicts = do
  perm <- permutations guests
  let pairs = zip perm (tail perm) ++ [(last perm, head perm)]
  guard $ all (not . conflicting) pairs
  return perm
  where
    conflicting (a, b) = (a, b) `elem` conflicts || (b, a) `elem` conflicts


-- Task 4 Result monad with warnings

data Result a = Failure String | Success a [String]

instance Show a => Show (Result a) where
  show (Failure msg)     = "Failure: " ++ msg
  show (Success v [])    = "Success: " ++ show v
  show (Success v ws)    = "Success: " ++ show v ++ " [warnings: " ++ show ws ++ "]"

-- (a)
instance Functor Result where
  fmap _ (Failure msg)    = Failure msg
  fmap f (Success v ws)   = Success (f v) ws

instance Applicative Result where
  pure x = Success x []
  Failure msg   <*> _             = Failure msg
  _             <*> Failure msg   = Failure msg
  Success f ws1 <*> Success x ws2 = Success (f x) (ws1 ++ ws2)

instance Monad Result where
  return = pure
  Failure msg   >>= _ = Failure msg
  Success v ws  >>= f = case f v of
    Failure msg      -> Failure msg
    Success v' ws'   -> Success v' (ws ++ ws')

-- (b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- (c)
validateAge :: Int -> Result Int
validateAge age
  | age < 0   = failure $ "Negative age: " ++ show age
  | age > 150 = do warn $ "Unusual age: " ++ show age; return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge



-- Task 5 Evaluator with simplification log

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

instance Show Expr where
  show (Lit n)   = show n
  show (Neg e)   = "(-" ++ show e ++ ")"
  show (Add l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Mul l r) = "(" ++ show l ++ " * " ++ show r ++ ")"

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)

simplify (Neg e) = do
  e' <- simplify e
  case e' of
    Neg inner -> tell ["Double negation: -(-e) -> e"] >> return inner
    _         -> return (Neg e')

simplify (Add l r) = do
  l' <- simplify l
  r' <- simplify r
  case (l', r') of
    (Lit 0, _)   -> tell ["Add identity: 0 + e -> e"]             >> return r'
    (_, Lit 0)   -> tell ["Add identity: e + 0 -> e"]             >> return l'
    (Lit a, Lit b) -> tell ["Constant fold: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a+b)]
                      >> return (Lit (a + b))
    _            -> return (Add l' r')

simplify (Mul l r) = do
  l' <- simplify l
  r' <- simplify r
  case (l', r') of
    (Lit 0, _)   -> tell ["Zero absorption: 0 * e -> 0"]          >> return (Lit 0)
    (_, Lit 0)   -> tell ["Zero absorption: e * 0 -> 0"]          >> return (Lit 0)
    (Lit 1, _)   -> tell ["Mul identity: 1 * e -> e"]             >> return r'
    (_, Lit 1)   -> tell ["Mul identity: e * 1 -> e"]             >> return l'
    (Lit a, Lit b) -> tell ["Constant fold: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a*b)]
                      >> return (Lit (a * b))
    _            -> return (Mul l' r')

