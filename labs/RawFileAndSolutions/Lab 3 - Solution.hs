{-# LANGUAGE TupleSections #-}

import Prelude hiding (Semigroup, Monoid)
import Data.List
import Data.Tuple

removeDuplicates1 :: Eq a => [a] -> [a]
removeDuplicates1 []     = []
removeDuplicates1 (x:xs) = x:removeDuplicates1 (remove x xs)
  where remove :: Eq a => a -> [a] -> [a]
        remove _ [] = []
        remove x (y:ys) | x==y      = remove x ys
                        | otherwise = y:remove x ys

removeDuplicates2 :: Eq a => [a] -> [a]
removeDuplicates2 []     = []
removeDuplicates2 (x:xs) = x:removeDuplicates2 (filter (x/=) xs)

removeDuplicatesBy :: (a -> a -> Bool) -> [a] -> [a]
removeDuplicatesBy eq []     = []
removeDuplicatesBy eq (x:xs) = x:removeDuplicatesBy eq (filter (not . eq x) xs)   

removeDuplicates3 :: Eq a => [a] -> [a]  
removeDuplicates3 = removeDuplicatesBy (==)          

type Relation a b = [(a,b)]

domRel :: Eq a => Relation a b -> [a]
domRel = nub . map fst

codRel :: Eq b => Relation a b -> [b]
codRel = nub . map snd

emptyRel :: Relation a b
emptyRel = []

unionRel :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b
unionRel r1 r2 = nub (r1 ++ r2)

intersectRel :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b
intersectRel r1 r2 = nub [ p | p <- r1, p `elem` r2 ]

intersectRel2 :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b
intersectRel2 r1 r2 = filter (\p -> p `elem` r2 ) r1

intersectRel3 r1 r2 = filter (`elem` r2) r1

converseRel :: (Eq a, Eq b) => Relation a b -> Relation b a
converseRel = nub . map swap

composeRel :: (Eq a, Eq b, Eq c) => Relation a b -> Relation b c -> Relation a c
composeRel r1 r2 = nub [ (x,z) | (x,y1) <- r1, (y2,z) <- r2, y1 == y2 ]

composeRel2 :: (Eq a, Eq b, Eq c) => Relation a b -> Relation b c -> Relation a c
composeRel2 r1 r2 = concat (map (\(x,y) -> map (\(_,z) -> (x,z)) (filter (\(y',_) -> y == y') r2)) r1)

composeRel3 :: (Eq a, Eq b, Eq c) => Relation a b -> Relation b c -> Relation a c
composeRel3 r1 r2 = concat (map (\(x,y) -> map ((x,) . snd) (filter ((y==) . fst) r2)) r1)

example1 :: Relation Int Int
example1 = [(1,2),(1,3),(2,5),(3,3),(3,4),(4,2),(4,3),(4,6)]

example2 :: Relation Int Int
example2 = [(2,5),(3,5),(6,5)]

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x
  | x == f x  = x
  | otherwise = fixedPoint f (f x)

tClosure :: Eq a => Relation a a -> Relation a a
tClosure r = fixedPoint (\s -> s `unionRel` (s `composeRel` r)) r

rtClosure :: Eq a => Relation a a -> Relation a a
rtClosure r = tClosure r `unionRel` nub [ (x,x) | x <- domRel r ++ codRel r ]

connected :: Eq a => Relation a a -> a -> a -> Bool
connected r x y = (x,y) `elem` rtClosure r

class Semigroup a where
  (+++) :: a -> a -> a
  
addAll1 :: Semigroup a => [a] -> a
addAll1 = foldl1 (+++)
  
instance Semigroup Bool where
  (+++) = (&&)
  
instance Semigroup Int where
  (+++) = (+)
  
instance Semigroup [a] where
  (+++) = (++)
  
instance Semigroup Integer where
  (+++) = min
  
class Semigroup a => Monoid a where
  zero :: a
  
instance Monoid Bool where 
  zero = True
  
instance Monoid Int where 
  zero = 0
  
instance Monoid [a] where 
  zero = []

addAll :: Monoid a => [a] -> a
addAll = foldl (+++) zero


  
  


