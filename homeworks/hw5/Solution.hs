import Control.Monad.State

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

