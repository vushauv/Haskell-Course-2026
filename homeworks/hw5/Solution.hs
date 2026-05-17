import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map


-- Task 1

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG


execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :) 
execInstr POP = do
    stack <- get
    case stack of
        (_:rest) -> put rest
        []       -> return ()

execInstr DUP = do
    stack <- get
    case stack of
        (x:rest) -> put (x : x : rest)
        []       -> return ()

execInstr SWAP = do
    stack <- get
    case stack of
        (x:y:rest) -> put (y : x : rest)
        _          -> return ()

execInstr ADD = do
    stack <- get
    case stack of
        (x:y:rest) -> put (x + y : rest)
        _          -> return ()

execInstr MUL = do
    stack <- get
    case stack of
        (x:y:rest) -> put (x * y : rest)
        _          -> return ()

execInstr NEG = do
    stack <- get
    case stack of
        (x:rest) -> put (negate x : rest)
        []       -> return ()



execProg :: [Instr] -> State [Int] ()
execProg []     = return ()
execProg (i:is) = execInstr i >> execProg is


runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []


-- Task 2


data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq  Expr Expr

eval :: Expr -> State (Map String Int) Int
eval (Num n)       = return n
eval (Var x)       = gets (Map.findWithDefault 0 x)
eval (Add e1 e2)   = (+)    <$> eval e1 <*> eval e2
eval (Mul e1 e2)   = (*)    <$> eval e1 <*> eval e2
eval (Neg e)       = negate <$> eval e
eval (Assign x e)  = do
    v <- eval e
    modify (Map.insert x v)
    return v
eval (Seq e1 e2)   = eval e1 >> eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

-- Task 3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just v  -> return v
        Nothing -> do
            result <- compute
            modify (Map.insert (i, j) result)
            return result
  where
    compute
        | i == 0    = return j
        | j == 0    = return i
        | xs !! (i-1) == ys !! (j-1) = editDistM xs ys (i-1) (j-1)
        | otherwise = do
            del  <- editDistM xs ys (i-1) j
            ins  <- editDistM xs ys i     (j-1)
            sub  <- editDistM xs ys (i-1) (j-1)
            return (1 + minimum [del, ins, sub])

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty


