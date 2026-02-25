import Data.List hiding (union)
import Data.Word
import Trace.Hpc.Util (Hash)
import System.Win32 (xBUTTON1)

type Byte = Word8

{- Please do the following:
   1. Add the following information:
      Name: Troy Neal
      Std#: 7643091
   2. Implement the functions in Haskell below.
   3. Once you are finished sent the file to mwinter@brocku.ca. You will
      receive a confirmation by email from me. After that you are free to
      leave.
   4. The test is out of 20 marks. Individual marks for each question 
      are given below.      

    Functions that you may need:

    fromEnum :: Enum a => a -> Int
        Converts an element of an enumerable type a to an Int.
      
    toEnum :: Enum a => Int -> a
        Converts an Int to an element of an enumerable type a.
   
    foldl :: (a -> b -> a) -> a -> [b] -> a
        foldl, applied to a binary operator, a starting value, and a list, 
        reduces the list using the binary operator, from left to right:
            foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
        
    concatMap :: (a -> [b]) -> [a] -> [b]
        Maps a function returning lists over a list and concatenates the results.
        
    minBound :: Bounded a => a
        Minimal value of a bounded type a.
        
    maxBound :: Bounded a => a
        Maximal value of a bounded type a.
        
    elem :: Eq a => a -> [a] -> Bool
        elem is the list membership predicate, usually written in infix
        form, e.g., x `elem` xs.
        
    nub :: Eq a => [a] -> [a]
        The nub function removes duplicate elements from a list. In particular, 
        it keeps only the first occurrence of each element.
-}

{- 
    Question 1 (10 marks, 1 mark for each of b)-e), 2 marks for each a) and f)-g)):
    This question focuses on calculating hash codes for elements of certain data types. 
    Remark:     Please note that we are using a single byte as the hash code for an
                element instead of "larger" type such as Int. This is done so that 
                printing a HashSet (see Question 2) can be done in reasonable time.
    a)  Define a class Hashable for hashable types. A type a is hashable if it already
        has an equality, i.e., is already an instance of the class Eq, and there is a 
        function hashcode :: a -> Byte converting an element of type a into its hash
        code (of type Byte).
    b)  Make the type Bool an instance of Hashable. The hash code for False is 0, and 
        for True is 1.
    c)  Make the type Int an instance of Hashable. The hash code of n is the remainder
        of n divided by 256 converted to a Byte. The conversion can be done by using the
        function toEnum (see above).
        Examples:   hashcode (25::Int)      == 25
                    hashcode (300::Int)     == 44
                    hashcode (-123::Int)    == 133
    d)  Make the type Char an instance of Hashable. A character can be converted to an
        Int by using the function fromEnum (see above). Then use the hashcode function for 
        Int to compute the hashcode of the character.
        Examples:   hashcode 'a'    == 97
                    hashcode '1'    == 49
                    hashcode '\n'   == 10
                    hashcode '\300' == 44
    e)  Make the type (a,b) of pairs from a and b an instance of Hashable assuming that the
        type a as well as the type b are already Hashable. Use the formula 31*x+y for the hash
        code where x is the hash code of the first and y is the hash code of the second 
        component of the pair.
        Examples:   hashcode ('a','b')              == 33
                    hashcode (-123::Int,300::Int)   == 71
                    hashcode (25::Int,'\t')         == 16
                    hashcode (('a','b'),'c')        == 98
    f)  Make the type (a,b,c) of triples from a, b and c an instance of Hashable assuming 
        that a, b and c are already Hashable. Implement the hashcode function by using the
        fact that nested pair ((a,b),c) is already an instance of Hashable.
        Examples:   hashcode ('a','b','c')          == 98
                    hashcode (12::Int,True,'a')     == 140
                    hashcode (25::Int,'\t',0::Int)  == 240
    g)  Make the type [a] of lists over a an instance of Hashable assuming that a is already
        Hashable. The empty list has hash code 0. Then use the formula 31*x+y from e where x is
        the hash code of the list up to the current point and y is the hash code of the current
        element. This can be done by using foldl.
        Examples:   hashcode "ab"               == 33
                    hashcode "abc"              == 98
                    hashcode ""                 == 0
                    hashcode ([1,2,3,4]::[Int]) == 66
                    hashcode ([-123,300]::[Int])== 71 
-}

class (Eq a) => Hashable a
    where hashcode :: a -> Byte
instance Hashable Bool
    where hashcode False = 0
          hashcode True  = 1
instance Hashable Int
    where hashcode x = toEnum( x `mod` 256)
instance Hashable Char
    where hashcode x = hashcode(fromEnum x)
instance (Hashable a,Hashable b) => Hashable (a,b)
    where hashcode (x,y) = 31 * hashcode x + hashcode y
instance (Hashable a,Hashable b, Hashable c) => Hashable (a,b,c)
    where hashcode (x,y,z) =  31 * hashcode(x,y) + hashcode z
instance (Hashable a) => Hashable [a]
    where hashcode = foldl (\h x -> 31 * h + hashcode x ) 0
    

    
{- 
    Question 2 (10 marks, 2 marks for each a-e)):
    The type HashSet a (see below) is the type of a set of elements from type a. Such a set
    stores its elements under their hash code. The set uses separate chaining for collision resolution. 
    Concretely, if s :: HashSet a, then s n is the list of all elements x in the 
    set s so that the hash code of x is equal to n. For example, the exampleSet1 is a set of 
    Int's containing the elements 266,10,305,353,97. The elements 97 and 353 are stored in 
    the list exampleSet 97 because both Int's (97 and 353) have a hash code of 97.
    a)  Define a function elements :: HashSet a -> [a] so that for s :: HashSet a the list 
        elements s is the list of all elements in the set s. 
        Hint:       Use concatMap and the list [minBound..maxBound] (or equivalently [0..255]).
        Example:    elements exampleSet1 == [266,10,305,353,97]
    b)  Define a function elemSet :: Hashable a => a -> HashSet a -> Bool that returns True iff
        the first parameter is in the set (second parameter), i.e., elemSet x s equals True iff
        x is in the list s (hashcode x).
        Hint:      Use the function elem.
        Examples:   == True
                   elemSet  49 exampleSet1 == False
                   elemSet  10 exampleSet1 == True
                   elemSet 522 exampleSet1 == False
                   elemSet 266 exampleSet1 == True
    c)  Define a function add :: Hashable a => a -> HashSet a -> HashSet a that adds an element
        to a HashSet.
        Hint:      In order to define this function start with
                   add x s b ...            (where x::a, s::HashSet a, b::Byte)
                   distinguish the cases whether hashcode x==b or not, and return the appropriate
                   list with elements from a.
                   (Note that a hash set is a function from Byte to [a]).
                   Use the function nub to remove potential duplicates in a list.
        Examples:  elements (add 11 exampleSet1) == [266,10,11,305,353,97]
                   elements (add 49 exampleSet1) == [266,10,49,305,353,97]
    d)  Define a function union: Hashable a => HashSet a -> HashSet a -> HashSet a that computes
        the union of two (hash) sets. 
        Hint:      In order to define this function start with
                   union s1 s2 b ...            (where s1, s2::HashSet a, b::Byte)
                   and return the appropriate list with elements from a.
                   (Note that a hash set is a function from Byte to [a]).
                   Use the function nub to remove potential duplicates in a list.
        Example:   elements (union exampleSet1 exampleSet2) == [266,10,522,305,49,80,353,97]
    e)  Define a function meet: Hashable a => HashSet a -> HashSet a -> HashSet a that computes
        the meet (or intersection) of two (hash) sets. 
        Hint:      In order to define this function start with
                   meet s1 s2 b ...            (where s1, s2::HashSet a, b::Byte)
                   and return the appropriate list with elements from a.
                   (Note that a hash set is a function from Byte to [a]).
        Example:   elements (meet exampleSet1 exampleSet2) == [305]
-}
    
type HashSet a = Byte -> [a]

elements :: HashSet a -> [a]
elements s = concatMap s [minBound.. maxBound]

elemSet :: Hashable a => a -> HashSet a -> Bool
elemSet x z = x `elem` z (hashcode x)

add :: Hashable a => a -> HashSet a -> HashSet a
add x s b 
 | hashcode x == b = nub (x: s b)
 | otherwise = s b

union :: Hashable a => HashSet a -> HashSet a -> HashSet a
union s1 s2 b = nub(s1 b ++ s2 b)

meet ::  Hashable a => HashSet a -> HashSet a -> HashSet a
meet s1 s2 b = [x | x <- s1 b, x `elem` s2 b]



exampleSet1 :: HashSet Int
exampleSet1 97 = [353,97]
exampleSet1 49 = [305]
exampleSet1 10 = [266,10]
exampleSet1 _  = []

exampleSet2 :: HashSet Int
exampleSet2 80 = [80]
exampleSet2 49 = [49,305]
exampleSet2 10 = [522]
exampleSet2 _  = []

