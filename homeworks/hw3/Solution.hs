import qualified Data.Map as Map
import Control.Monad (foldM)

-- Task 1 Maybe Monad

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
