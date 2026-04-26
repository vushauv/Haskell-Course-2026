import qualified Data.Map as Map

-- Task 1 Maybe Monad

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map.Map Pos (Map.Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = Map.lookup pos maze >>= Map.lookup dir
