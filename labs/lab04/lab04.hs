import Data.Word

type Byte = Word8

class Eq a => Hashable a where
    hashcode :: a -> Byte

{- Make the type Bool an instance of Hashable. The hash code for False is 0, and 
for True is 1 .
-}

instance Hashable Bool where
    hashcode False = 0
    hashcode _ = 1

{-Make the type Int an instance of Hashable. The hash code of n is the remainder
        of n divided by 256 converted to a Byte.
-}  

instance Hashable Int where
    hashcode i = toEnum(i `mod` 256)

{-
Make the type (a,b) of pairs from a and b an instance of Hashable assuming that the
        type a as well as the type b are already Hashable. Use the formula 31*x+y for the hash
        code where x is the hash code of the first and y is the hash code of the second 
        component of the pair.
-} 

instance (Hashable a, Hashable b) => Hashable(a,b)
    where hashcode (x,y) = 31 * hashcode(x) + hashcode(y)

 -- Char wasn't mentioned but its usefull.
instance Hashable Char
    where hashcode x = hashcode(fromEnum x)
{-
Make the type (a,b,c) of triples from a, b and c an instance of Hashable assuming 
        that a, b and c are already Hashable. Implement the hashcode function by using the
        fact that nested pair ((a,b),c) is already an instance of Hashable.
        Examples:   hashcode ('a','b','c')          == 98
                    hashcode (12::Int,True,'a')     == 140
                    hashcode (25::Int,'\t',0::Int)  == 240
-}

instance (Hashable a, Hashable b, Hashable c) => Hashable(a,b,c)
    where hashcode (x,y,z) = 31 * hashcode(x,y) + hashcode z

    
{-  Make the type [a] of lists over a an instance of Hashable assuming that a is already
        Hashable. The empty list has hash code 0. Then use the formula 31*x+y from e where x is
        the hash code of the list up to the current point and y is the hash code of the current
        element. This can be done by using foldl.
        Examples:   hashcode "ab"               == 33
                    hashcode "abc"              == 98
                    hashcode ""                 == 0
                    hashcode ([1,2,3,4]::[Int]) == 66
                    hashcode ([-123,300]::[Int])== 71 -}
 
instance (Hashable a) => Hashable [a]
    where hashcode xs = foldl (\h x -> 31 * h + hashcode x) 0 xs

type Hashtable a b = Byte -> [(a,b)]

emptyHT :: Hashtable a b
emptyHT _ = []

putHT :: Hashable a => Hashtable a b -> a -> b -> Hashtable a b
putHT f k v n 
 | hashcode k == n = (k,v): filter (\(k', _) -> k' /= k) (f n)
 | otherwise = f n

getHT :: Hashable a => Hashtable a b -> a -> Maybe b
getHT f k = lookup k (f (hashcode k))

entrySetHT :: Hashtable a b -> [(a,b)]
entrySetHT f = concatMap f [minBound..maxBound]

keySetHT :: Hashtable a b -> [a]
keySetHT f = [x | (x,_) <- entrySetHT f]

-- Either version works.
keySetHT' :: Hashtable a b -> [a]
keySetHT' f = map fst (entrySetHT f)

valueCollectionHT :: Hashtable a b -> [b]
valueCollectionHT f = map snd (entrySetHT f)

ht1 :: Hashtable Int String
ht1 = putHT (putHT emptyHT 1 "John") 2 "Steve"


--           Center   x     y   radius             c1x   c1y   c2x   c2y 
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

area :: Shape -> Float
area (Circle x y r) = pi * r^2
area (Rectangle x1 y1 x2 y2) = abs(x1-x2) * abs(y1-y2)

data Tree a b = Leaf b | Node a [Tree a b] deriving Eq

treeExample :: Tree Int Bool
treeExample = Node 1 [Leaf True,Node 5 [Leaf False],Node 2 [Leaf True,Leaf True]]

showTree :: (Show a, Show b) => Int -> Tree a b -> String
showTree n (Leaf b) = replicate " " n ++ show b + show "\n"
showTree n (Node v l) = replicate " " n ++ show v + show "\n" ++ concatMap(showTree (n+2)) l

instance (Show a, Show b) => Show (Tree a b) where
    show = showTree 0

{--

    
mapTree :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d


foldTree :: (a -> [c] -> c) -> (b -> c) -> Tree a b -> c


size :: Tree a b -> Int


depth :: Tree a b -> Int


allTree :: (a -> Bool) -> (b -> Bool) -> Tree a b -> Bool


anyTree :: (a -> Bool) -> (b -> Bool) -> Tree a b -> Bool


toList :: Tree a b -> [Either a b]


data NOp = Succ | Add | Mult | If deriving (Eq,Show)

executeNOp :: NOp -> [Int] -> Int


type AExpr = Tree NOp Int

expExample :: AExpr
expExample = Node Mult [Leaf 2,Node Add [Leaf 3,Node Succ [Leaf 4]]]

eval :: AExpr -> Int
--}
