import Data.Char
import Data.List
import Text.Read(readMaybe)
import Control.Monad
import Control.Monad.Trans

-- State transformer as it is in the libraries

newtype StateT a m b = StateT (a -> m (a,b))

instance MonadTrans (StateT a) where
  lift mval = StateT (\st -> do val <- mval
                                return (st,val))

instance Functor f => Functor (StateT a f) where
    fmap g (StateT trans) = StateT (\st -> fmap (\(newSt,x) -> (newSt,g x)) (trans st))
        
instance Monad m => Applicative (StateT a m) where
    pure x                                = StateT (\st -> pure (st,x))
    (StateT mtrans1) <*> (StateT mtrans2) = StateT (\st -> do (newSt1,f) <- mtrans1 st 
                                                              (newSt2,x) <- mtrans2 newSt1 
                                                              return (newSt2, f x))

instance Monad m => Monad (StateT a m) where 
    return                = pure 
    (StateT mtrans) >>= f = StateT (\st -> do (newSt,x) <- mtrans st
                                              let (StateT newTrans) = f x
                                              newTrans newSt)

-- Some functions for StateT (also already implemented in the libraries)
                               
get :: Monad m => StateT s m s
get = StateT (\st -> return (st,st)) 

put :: Monad m => s -> StateT s m ()
put s = StateT (const (return (s,()))) 

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT (\s -> return (f s, ()))

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT (StateT mtrans) s = fmap snd (mtrans s)

-- An example

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

type Position = (Int,Int)
type Maze = [[Bool]]
type ExitLocation = Position
type World = (Maze,ExitLocation)
type GameState = (Position,World)

inMaze :: Maze -> Position -> Bool
inMaze maze (x,y) = if 0<=y && y<length maze 
                    then let row = maze!!y in
                         if 0<=x && x<length row 
                         then row!!x
                         else False
                    else False

data Direction = North | East | South | West deriving Show

instance Read Direction where
    readsPrec _ ('n':rest) = [(North,rest)]
    readsPrec _ ('e':rest) = [(East,"")]
    readsPrec _ ('s':rest) = [(South,"")]
    readsPrec _ ('w':rest) = [(West,"")]
    readsPrec _ _   = []
    
move :: Position -> Direction -> Position
move (x,y) North = (x,y-1)
move (x,y) East  = (x+1,y)
move (x,y) South = (x,y+1)
move (x,y) West  = (x-1,y)

canMoveInMaze :: Maze -> Position -> Direction -> Bool
canMoveInMaze maze pos dir = inMaze maze (move pos dir) 

myProg :: StateT GameState IO ()
myProg = do (pos,(maze,exit)) <- get
            lift $ putStrLn ("You are at " ++ show pos ++ ".")
            mMove <- lift $ fmap (readMaybe . trim) getLine
            case mMove of
                Nothing  -> 
                    do  lift $ putStrLn "Illegal input."
                        myProg
                Just dir -> 
                    do  if canMoveInMaze maze pos dir 
                        then do let newPos = move pos dir
                                if newPos==exit
                                then do lift $ putStrLn "You escaped.\n Good bye."
                                        return ()
                                else do lift $ putStrLn "Good move."
                                        put (move pos dir,(maze,exit))
                                        myProg
                        else do lift $ putStrLn "You cannot go there."
                                myProg
                        
myMaze = [  [ True,  True,  True,  True, True ],
            [ True, False,  True, False, True ],
            [ True, False,  True, False, True ],
            [ True, False, False, False, True ],
            [ True,  True,  True,  True, True ] ]
                    
main :: IO ()
main = evalStateT myProg ((0,0),(myMaze,(2,2)))