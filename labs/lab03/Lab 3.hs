{-# LANGUAGE TupleSections #-}

import Prelude hiding (Semigroup, Monoid)
import Data.List
import Data.Tuple

example2 = [(1,3),(6,7),(7,9),(10,42),(100,23)]
example1 = [(1,3),(2,3),(2,4),(1,1)]

{-- 
            RECURSION    
 --}

-- Recursion with remove using recursion 
removeDuplicates1 :: Eq a => [a] -> [a]
removeDuplicates1 [] = []
removeDuplicates1 (x:xs) = x : removeDuplicates1 (remove x xs)
    where remove :: (Eq a) => a -> [a] -> [a] -- Type information is optional
          remove _ [] = []
          remove c (q:qs)
           | c == q = remove c qs
           | otherwise = q : remove c qs

-- Recursion with remove using filtering
removeDuplicates1' :: Eq a => [a] -> [a]
removeDuplicates1' [] = []
removeDuplicates1' (x:xs) = x : removeDuplicates1' (filter (x/=) xs) -- same as: (filter (\c -> c /= x) xs)

-- Remove duplicates, by passing a comparison function
removeDuplicatesBy :: (a -> a -> Bool) -> [a] -> [a]
removeDuplicatesBy _ [] = []
removeDuplicatesBy compf (x:xs) = x : removeDuplicatesBy compf (filter (not. compf x) xs) -- Same as: (\c -> not(compf c x)) xs)

{--
            RELATIONS
--}
type Relation a b = [(a,b)]
-- Domain
domRel :: Eq a => Relation a b -> [a]
domRel = nub. map fst

--CoDomain
codRel :: Eq b => Relation a b -> [b]
codRel = nub. map snd


emptyRel :: Relation a b
emptyRel = []

unionRel :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b
unionRel r1 r2 = nub (r1++r2)

intersectRel :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b
intersectRel r1 r2 = [ p| p <- r1, p `elem` r2]

intersectRel' :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b
intersectRel' r1 r2 = filter (`elem` r2) r1

-- Converse Relation Manual Swap
converseRel :: (Eq a, Eq b) => Relation a b -> Relation b a
converseRel r1 = map (\(x,y) -> (y,x)) r1

-- Converse Relation using swap from Data.Tuple
converseRel' :: (Eq a, Eq b) => Relation a b -> Relation b a
converseRel' = map swap

-- Ran Out Of Time
--composeRel :: (Eq a, Eq b, Eq c) => Relation a b -> Relation b c -> Relation a c


{-- Doesn't Quite Work -- Example of Let
converseRel' :: (Eq a, Eq b) => Relation a b -> Relation b a
converseRel' r1 = zip (codRel r1) (domRel r1)

converseRel'' :: (Eq a, Eq b) => Relation a b -> Relation b a
converseRel'' r1 = let cod = codRel r1
                       dom = domRel r1
                    in zip cod dom
--}


{-- Remaining Implementations 

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

--}