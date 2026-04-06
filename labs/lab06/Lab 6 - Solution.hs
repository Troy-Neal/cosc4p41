import Data.List

data State a b = State (a -> (a,b))

instance Functor (State a) where
    fmap f (State trans) = State (\st -> let (newSt,x) = trans st in (newSt,f x))

instance Applicative (State a) where
    pure x                            = State (\st -> (st,x))
    (State trans1) <*> (State trans2) = State (\st -> let (newSt1,f) = trans1 st
                                                          (newSt2,x) = trans2 newSt1 
                                                      in (newSt2, f x))

instance Monad (State a) where 
 (State trans) >>= f = State (\st -> let (newSt,x)        = trans st
                                         (State newTrans) = f x
                                     in newTrans newSt)
                                      
getState :: State a a
getState = State (\st -> (st,st))  

setState :: a -> State a ()
setState st = State (const (st,()))

runState :: State a b -> a -> b 
runState (State trans) st = snd (trans st) 

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

type Table a = [a]                                  
                                      
numberTree :: Eq a => Tree a -> State (Table a) (Tree Int) 
numberTree Nil = return Nil 
numberTree (Node x t1 t2) = do st <- getState 
                               let (newSt,num) = numberNode x st
                               setState newSt
                               nt1 <- numberTree t1 
                               nt2 <- numberTree t2
                               return (Node num nt1 nt2) 

numberNode :: Eq a => a -> Table a -> (Table a , Int) 
numberNode x table = maybe (table++[x],length table) (\n -> (table,n)) (elemIndex x table) 

numTree :: Eq a => Tree a -> Tree Int
numTree t = runState (numberTree t) []

exTree :: Tree String
exTree = Node "Moon" (Node "Ahmet" Nil Nil) (Node "Dweezil" (Node "Ahmet" Nil Nil) (Node "Moon" Nil Nil))

-------------------------------------------------------------

type MIO = State (String,String)

mputStr :: String -> MIO ()
mputStr s = State (\(inS,outS) -> ((inS,outS++s),()))

mputStrLn  :: String -> MIO ()
mputStrLn s = mputStr (s++"\n")

mprint :: Show a => a -> MIO ()
mprint x = mputStrLn (show x)

mgetChar :: MIO Char
mgetChar = State (\(inS,outS) -> ((tail inS,outS),head inS))

mgetLine :: MIO String
mgetLine = State (\(inS,outS) -> let (Just index) = elemIndex '\n' inS
                                     (res,inS')   = splitAt index inS
                                 in ((tail inS',outS),res))
                                 
runMIO :: MIO () -> (String,String) -> IO ()
runMIO (State trans) st = do let ((_,outS),_) = trans st
                             putStr outS
                                 
exStreams = ("5stressed\nHello\n","")

myProg :: MIO ()
myProg = do mgetChar
            s1 <- mgetLine
            
            mputStrLn (reverse s1)
            s2 <- mgetLine
            mprint 5
            mputStrLn s2
           
run = runMIO myProg exStreams




