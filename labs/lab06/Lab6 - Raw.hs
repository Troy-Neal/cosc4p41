import Data.List

data State a b = State (a -> (a,b))


-- make State an instance of Functor 


-- make State an instance of Applicative 


-- make State and instance of Monad 



getState :: State a a


setState :: a -> State a ()


runState :: State a b -> a -> b 


data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

type Table a = [a]                                  
                                      
numberTree :: Eq a => Tree a -> State (Table a) (Tree Int) 


numberNode :: Eq a => a -> Table a -> (Table a , Int) 
 

numTree :: Eq a => Tree a -> Tree Int


exTree :: Tree String
exTree = Node "Moon" (Node "Ahmet" Nil Nil) (Node "Dweezil" (Node "Ahmet" Nil Nil) (Node "Moon" Nil Nil))

-------------------------------------------------------------

type MIO = State (String,String)

mputStr :: String -> MIO ()


mputStrLn  :: String -> MIO ()


mprint :: Show a => a -> MIO ()


mgetChar :: MIO Char


mgetLine :: MIO String

                                 
runMIO :: MIO () -> (String,String) -> IO ()

                                 
exStreams = ("5stressed\nHello\n","")

myProg :: MIO ()

           
run = runMIO myProg exStreams




