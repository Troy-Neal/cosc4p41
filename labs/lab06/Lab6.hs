import Data.List

data State a b = State (a -> (a,b))

-- make State an instance of Functor 
instance Functor (State a) where
    fmap f (State tran) = State (\st -> let (newSt, x) = tran st in (newSt, f x))
-- make State an instance of Applicative 

instance Applicative (State a) where
    pure x = State (\st -> (st, x))
    (State f) <*> (State g) = State (\st -> let (st', h) = f st
                                                (st'', x) = g st' 
                                                in (st'', h x))

-- make State and instance of Monad 
instance Monad (State a) where
    return = pure
    (State tran) >>= f = State (\st -> let (st', x) = tran st
                                           (State tran2) = f x 
                                       in (tran2 st'))

getState :: State a a
getState = State (\st -> (st, st))

setState :: a -> State a ()
setState st = State (const (st, ())) 

runState :: State a b -> a -> b 
runState (State tran) st = let (_, x) = tran st in x

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

type Table a = [a]                                  
                                      
numberTree :: Eq a => Tree a -> State (Table a) (Tree Int) 
numberTree Nil = return Nil
numberTree (Node x l r) = do
    table <- getState
    let (newTable, num) = numberNode x table
    setState newTable
    newL <- numberTree l
    newR <- numberTree r
    return (Node num newL newR)

numberNode :: Eq a => a -> Table a -> (Table a , Int) 
numberNode n table = maybe (table ++ [n], length table) (\n -> (table, n)) (elemIndex n table)

numTree :: Eq a => Tree a -> Tree Int
numTree tr = runState (numberTree tr) []

exTree :: Tree String
exTree = Node "Moon" (Node "Ahmet" Nil Nil) (Node "Dweezil" (Node "Ahmet" Nil Nil) (Node "Moon" Nil Nil))

-------------------------------------------------------------

-- type MIO = State (String,String)

-- mputStr :: String -> MIO ()


-- mputStrLn  :: String -> MIO ()


-- mprint :: Show a => a -> MIO ()


-- mgetChar :: MIO Char


-- mgetLine :: MIO String

                                 
-- runMIO :: MIO () -> (String,String) -> IO ()

                                 
-- exStreams = ("5stressed\nHello\n","")

-- myProg :: MIO ()

           
-- run = runMIO myProg exStreams




