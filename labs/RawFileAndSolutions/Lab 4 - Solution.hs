import Data.Word

type Byte = Word8

class Eq a => Hashable a where
    hashcode :: a -> Byte
    
instance Hashable Bool where
    hashcode False = 0
    hashcode True  = 1
    
instance Hashable Int where
    hashcode n = toEnum (n `mod` 256)
    
instance Hashable Char where
    hashcode = hashcode . fromEnum
 --   hashcode c = hashcode (fromEnum c)
 --   hashcode c = hashcode $ fromEnum c
 
instance (Hashable a, Hashable b) => Hashable (a,b) where
    hashcode (x,y) = 31 * hashcode x + hashcode y
    
instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    hashcode (x,y,z) = hashcode ((x,y),z)
    
instance Hashable a => Hashable [a] where
    hashcode = foldl (\h x -> 31 * h + hashcode x) 0 
    
type Hashtable a b = Byte -> [(a,b)]

entrySetHT :: Hashtable a b -> [(a,b)]
entrySetHT f = concatMap f [minBound .. maxBound]

keySetHT :: Hashtable a b -> [a]
keySetHT f = map fst (entrySetHT f)
--keySetHT = map fst . entrySetHT

valueCollectionHT :: Hashtable a b -> [b]
valueCollectionHT = map snd . entrySetHT

emptyHT :: Hashtable a b
emptyHT _ = []
--emptyHT = const []

getHT :: Hashable a => Hashtable a b -> a -> Maybe b
getHT f k = lookup k (f (hashcode k))

putHT :: Hashable a => Hashtable a b -> a -> b -> Hashtable a b
putHT f k v n | n == hashcode k = (k,v):filter (\(k',_) -> k' /= k) (f n)
              | otherwise       = f n
              
              

data Tree a b = Leaf b | Node a [Tree a b] deriving Eq

treeExample :: Tree Int Bool
treeExample = Node 1 [Leaf True,Node 5 [Leaf False],Node 2 [Leaf True,Leaf True]]

showTree :: (Show a, Show b) => Int -> Tree a b -> String
showTree n (Leaf b)   = replicate n ' ' ++ show b ++ "\n"
showTree n (Node a l) = replicate n ' ' ++ show a ++ "\n" ++ concatMap (showTree (n+2)) l 

instance (Show a, Show b) => Show (Tree a b) where
    show = showTree 0
    
mapTree :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
mapTree f g (Leaf b)   = Leaf (g b)
mapTree f g (Node a l) = Node (f a) (map (mapTree f g) l)

foldTree :: (a -> [c] -> c) -> (b -> c) -> Tree a b -> c
foldTree f g (Leaf b)   = g b
foldTree f g (Node a l) = f a (map (foldTree f g) l)

size :: Tree a b -> Int
size = foldTree (\_ l -> 1+sum l) (const 1)

depth :: Tree a b -> Int
depth = foldTree (\_ l -> 1+maximum l) (const 1)

allTree :: (a -> Bool) -> (b -> Bool) -> Tree a b -> Bool
allTree pa pb = foldTree (\a l -> pa a && and l) pb 

anyTree :: (a -> Bool) -> (b -> Bool) -> Tree a b -> Bool
anyTree pa pb = foldTree (\a l -> pa a || or l) pb 

toList :: Tree a b -> [Either a b]
toList = foldTree (\a l -> Left a:concat l) (\b -> [Right b])

data NOp = Succ | Add | Mult | If deriving (Eq,Show)

executeNOp :: NOp -> [Int] -> Int
executeNOp Succ [n]     = n+1
executeNOp Add  [m,n]   = m+n
executeNOp Mult [m,n]   = m*n
executeNOp If   [m,n,p] = if m==0 then n else p

type AExpr = Tree NOp Int

expExample :: AExpr
expExample = Node Mult [Leaf 2,Node Add [Leaf 3,Node Succ [Leaf 4]]]

eval :: AExpr -> Int
eval = foldTree executeNOp id









