{-# LANGUAGE TupleSections #-}

import Prelude hiding (Semigroup, Monoid)
import Data.List
import Data.Tuple

{--
removeDuplicates1 :: Eq a => [a] -> [a]


removeDuplicates2 :: Eq a => [a] -> [a]


removeDuplicatesBy :: (a -> a -> Bool) -> [a] -> [a]
  

removeDuplicates3 :: Eq a => [a] -> [a]  
       

type Relation a b = [(a,b)]

domRel :: Eq a => Relation a b -> [a]


codRel :: Eq b => Relation a b -> [b]


emptyRel :: Relation a b


unionRel :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b


intersectRel :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b


intersectRel2 :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b


intersectRel3 r1 r2 = filter (`elem` r2) r1

converseRel :: (Eq a, Eq b) => Relation a b -> Relation b a


composeRel :: (Eq a, Eq b, Eq c) => Relation a b -> Relation b c -> Relation a c


composeRel2 :: (Eq a, Eq b, Eq c) => Relation a b -> Relation b c -> Relation a c


composeRel3 :: (Eq a, Eq b, Eq c) => Relation a b -> Relation b c -> Relation a c


example1 :: Relation Int Int


example2 :: Relation Int Int


fixedPoint :: Eq a => (a -> a) -> a -> a


tClosure :: Eq a => Relation a a -> Relation a a


rtClosure :: Eq a => Relation a a -> Relation a a


connected :: Eq a => Relation a a -> a -> a -> Bool


class Semigroup a where

  
addAll1 :: Semigroup a => [a] -> a

  
instance Semigroup Bool where
 
  
instance Semigroup Int where

  
instance Semigroup [a] where

  
instance Semigroup Integer where

  
class Semigroup a => Monoid a where

  
instance Monoid Bool where 

  
instance Monoid Int where 

  
instance Monoid [a] where 


addAll :: Monoid a => [a] -> a

-}

  
  


