import Data.List
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Identity

divMaybe0 :: Int -> Int -> Maybe Int

              
myProg0 :: Int -> Int -> Int -> Maybe Int
myProg0 x y z = do d1 <- divMaybe0 (x+y) z
                   d2 <- divMaybe0 (x+z) y
                   d3 <- divMaybe0 (y+z) x
                   return (d1+d2+d3)  
                                          
{- ------------------------------------------------------------------ -}

newtype MaybeT m a = MaybeT (m (Maybe a))

--extractMaybeT :: MaybeT m a -> m (Maybe a)


--instance Functor m => Functor (MaybeT m) where

  
--instance Monad m => Applicative (MaybeT m) where

                       
--instance Monad m => Monad (MaybeT m) where


--instance MonadTrans MaybeT where
  
divMaybe' :: Monad m => m() -> Int -> Int -> MaybeT m Int
divMaybe' m x y 
  | y==0      = MaybeT $ m >> return Nothing
  | otherwise = MaybeT $ return (Just (x `div` y))

  
myProg1 :: Int -> Int -> Int -> MaybeT IO Int
myProg1 x y z = do d1 <- divMaybe' (putStrLn "Error: z=0") (x+y) z
                   lift (print d1)
                   d2 <- divMaybe' (putStrLn "error: y=0") (x+z) y
                   lift (print d2)
                   d3 <- divMaybe' (putStrLn "error: x=0") (y+z) x
                   lift (print d3)
                   return (d1+d2+d3)
                   
{- ------------------------------------------------------------------- -}
                  
newtype CompM m1 m2 a = CompM { runCompM :: m2 (m1 a) }

--myLift :: (Monad m1, Functor m2) => m2 a -> CompM m1 m2 a


--instance (Functor m1, Functor m2) => Functor (CompM m1 m2) where

  
--instance (Applicative m1, Monad m2) => Applicative (CompM m1 m2) where


class Swapper f where
  swapper :: Monad m => f (m a) -> m (f a)
  
instance Swapper Maybe where
    swapper Nothing  = return Nothing
    swapper (Just m) = m >>= return . Just
    
instance (Monad m1, Monad m2, Swapper m1) => Monad (CompM m1 m2) where
    return          = CompM . return . return
    (CompM m) >>= f = CompM (m >>= fmap join . swapper . fmap (runCompM . f)) 
  
divMaybe'' :: Monad m => m() -> Int -> Int -> CompM Maybe m Int
divMaybe'' m x y 
  | y==0      = CompM $ m >> return Nothing
  | otherwise = CompM $ return (Just (x `div` y))
  
myProg2 :: Int -> Int -> Int -> CompM Maybe IO Int
myProg2 x y z = do d1 <- divMaybe'' (putStrLn "Error: z=0") (x+y) z
                   myLift (print d1)
                   d2 <- divMaybe'' (putStrLn "error: y=0") (x+z) y
                   myLift (print d2)
                   d3 <- divMaybe'' (putStrLn "error: x=0") (y+z) x
                   myLift (print d3)
                   return (d1+d2+d3)

{- ------------------------------------------------------------------- -}

newtype StateT a m b = StateT { runStateT :: a -> m (a,b) }

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
                                      
getState :: Monad m => StateT a m a
getState = StateT (\st -> return (st,st))  

setState :: Monad m => a -> StateT a m ()
setState st = StateT (const $ return (st,()))

runState :: Monad m => StateT a m b -> a -> m b 
runState (StateT mtrans) st = fmap snd (mtrans st) 

type State a b = StateT a Identity b 

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

type Table a = [a]                                  
  
numberNode :: (Eq a, Show a) => a -> StateT (Table a) IO Int 
numberNode x = do st <- getState
                  case elemIndex x st of 
                    Nothing -> do lift $ putStrLn (msg x st)
                                  setState (st++[x]) 
                                  return (length st)
                    Just y  -> return y
  where msg x st ="New value " ++ show x ++ " detected. Number " ++ show (length st) ++ " assigned."

numberTree :: (Eq a, Show a) => Tree a -> StateT (Table a) IO (Tree Int) 
numberTree Nil = return Nil 
numberTree (Node x t1 t2) = do num <- numberNode x
                               nt1 <- numberTree t1 
                               nt2 <- numberTree t2
                               return (Node num nt1 nt2)

numTree :: (Eq a, Show a) => Tree a -> IO (Tree Int)
numTree t = runState (numberTree t) []                               

exTree :: Tree String
exTree = Node "Moon" (Node "Ahmet" Nil Nil) (Node "Dweezil" (Node "Ahmet" Nil Nil) (Node "Moon" Nil Nil))


