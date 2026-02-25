import Data.Word

type Byte = Word8

class Eq a => Hashable a where
    hashcode :: a -> Byte

{- Make the type Bool an instance of Hashable. The hash code for False is 0, and 
for True is 1 .
-}


{-Make the type Int an instance of Hashable. The hash code of n is the remainder
        of n divided by 256 converted to a Byte.
-}  

    

{-
Make the type (a,b) of pairs from a and b an instance of Hashable assuming that the
        type a as well as the type b are already Hashable. Use the formula 31*x+y for the hash
        code where x is the hash code of the first and y is the hash code of the second 
        component of the pair.
-} 
 

{-
Make the type (a,b,c) of triples from a, b and c an instance of Hashable assuming 
        that a, b and c are already Hashable. Implement the hashcode function by using the
        fact that nested pair ((a,b),c) is already an instance of Hashable.
        Examples:   hashcode ('a','b','c')          == 98
                    hashcode (12::Int,True,'a')     == 140
                    hashcode (25::Int,'\t',0::Int)  == 240
-}

    

{-  Make the type [a] of lists over a an instance of Hashable assuming that a is already
        Hashable. The empty list has hash code 0. Then use the formula 31*x+y from e where x is
        the hash code of the list up to the current point and y is the hash code of the current
        element. This can be done by using foldl.
        Examples:   hashcode "ab"               == 33
                    hashcode "abc"              == 98
                    hashcode ""                 == 0
                    hashcode ([1,2,3,4]::[Int]) == 66
                    hashcode ([-123,300]::[Int])== 71 -}
 
    

type Hashtable a b = Byte -> [(a,b)]

entrySetHT :: Hashtable a b -> [(a,b)]


keySetHT :: Hashtable a b -> [a]


valueCollectionHT :: Hashtable a b -> [b]


emptyHT :: Hashtable a b


getHT :: Hashable a => Hashtable a b -> a -> Maybe b


putHT :: Hashable a => Hashtable a b -> a -> b -> Hashtable a b


data Tree a b = Leaf b | Node a [Tree a b] deriving Eq

treeExample :: Tree Int Bool
treeExample = Node 1 [Leaf True,Node 5 [Leaf False],Node 2 [Leaf True,Leaf True]]

showTree :: (Show a, Show b) => Int -> Tree a b -> String


instance (Show a, Show b) => Show (Tree a b) where
    show = showTree 0
    
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

