{- Please do the following:
   1. Add the following information:
      Name: Troy Neal
      Std#: 7643091
   2. Implement the functions in Haskell below.
   3. Once you are finished sent the file to ra16pz@brocku.ca. You will
      receive a confirmation by email. After that you are free to
      leave/log out.
   4. The test is out of 20 marks. Individual marks for each question 
      are given below.  
      
   Functions that you may need:

   mod :: Integral a => a -> a -> a
      integer modulus, e.g., 10 `mod` 3 == 1
      
   sum :: Num a => [a] -> a
         The sum function computes the sum of a finite list of numbers.
   
   map :: (a -> b) -> [a] -> [b]
      map f xs is the list obtained by applying f to each element of xs, i.e., 
        map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
        
   filter :: (a -> Bool) -> [a] -> [a]
      filter p l returns the list of those elements that satisfy the predicate, i.e.,
        filter p xs = [ x | x <- xs, p x]
      
   foldl :: (a -> b -> a) -> a -> [b] -> a
      foldl, applied to a binary operator, a starting value, and a list, 
      reduces the list using the binary operator, from left to right:
        foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
        
   foldr :: (a -> b -> b) -> b -> [a] -> b
      foldr, applied to a binary operator, a starting value, and a list, 
      reduces the list using the binary operator, from right to left:
        foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-}

{- 
   Question 1 (2 marks):
   Use pattern matching and recursion to define a function merge that merges
   to ordered list so that the result is also ordered.
   Example: merge [1,2,6] [3,7,8] == [1,2,3,6,7,8]
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] (x:xs) = (x:xs)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys)
 | x <= y = (x:merge xs (y:ys))
 | otherwise = (y:merge (x:xs) ys)

{- 
   Question 2 (2 marks):
   Write a recursive rangeProduct which computes the product
   m*(m-1)*...*(n+1)*n for the given parameters m and n. 
   (If m<n, the function returns n)
   Examples: rangeProduct 5 1 == 120
             rangeProduct 6 4 == 120
             rangeProduct 7 3 == 2520
             rangeProduct 3 7 == 7
-}

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
 | m <= n  = n
 | otherwise = m * rangeProduct (m-1) n


{- 
   Question 3 (2 marks):
   Using list comprehension, write a function factors :: Int -> [Int] that
   computes all positive factors of an positive input in ascending order. 
   If the input is 0 or negative, the function should return the empty list.
   Examples: factors 12 == [1,2,3,4,6,12]
             factors 9  == [1,3,9]
             factors 0  == []
-}

factors :: Int -> [Int]
factors n
 | n <= 0 = []
 | otherwise = [x | x <- [1..n], n `mod` x == 0]

{-
   Question 4 (2 marks):
   Using list comprehension, write a function perfects :: Int -> [Int] that
   lists all perfects numbers less or equal to the parameter in ascending order. 
   A perfect number is a positive number x so that the sum of its factors equals 
   2*x. You may use the function from the previous question.
   Examples: perfects 500   == [6,28,496]
             perfects 10000 == [6,28,496,8128] (takes longer)
-}

perfects :: Int -> [Int]
perfects n
 | n <= 0 = []
 | otherwise = [x | x <- [1..n], sum (factors(x)) == (2*x)]

{-
   Question 5 (2 marks):
   Use the function map to define replaceIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
   so that replaceIf p f l replaces every element x in the list l by f x if p x is true
   and leaves x unchanged if p x is false.   
   Examples: replaceIf even (+1) [1,2,3,4,5,6] == [1,3,3,5,5,7]
             replaceIf odd (2*) [1,2,3,4,5,6]  == [2,2,6,4,10,6]
-}

replaceIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceIf p f = map (\x -> if p x then f x else x)
{-
   Question 6 (2 marks):
   Use the functions map and filter to define a function mapIf :: (a -> Bool) -> (a -> b) -> [a] -> [b]
   so that the list mapIf p f l has the elements f x for all elements x from l where p x
   is true. 
   (Remark: The function from Question 5 will not help to implement this.)
   Examples: mapIf even (+1) [1,2,3,4,5,6] == [3,5,7]
             mapIf odd (2*) [1,2,3,4,5,6]  == [2,6,10]

-}

mapIf :: (a -> Bool) -> (a -> b) -> [a] -> [b]
mapIf p f = map f . filter p

{-
   Question 7 (2 marks):
   Use foldl to write a function bit2sInt :: [Bool] -> Int that converts the list
   of bits seen as a binary number in its decimal representation.
   Examples:  bits2Int [True,False,True]            == 5  (101b = 5d)
              bits2Int [True,False,False,True,True] == 19 (10011b == 19d)
              bits2Int []                           == 0
-}

bits2Int :: [Bool] -> Int
bits2Int = foldl (\acc x -> acc * 2 + if x then 1 else 0) 0

{- 
   Question 8 (3 marks):
   Use a fold operation (foldl or foldr) to implement a function apply that 
   applies every function from a list of functions of type a -> a to an a element
   starting with the last function of the list. If the list of function is empty
   you should get the original element.
   Examples: apply [(+1),(2*)] 5      == 11 (2*5+1 = 11)
             apply [(2*),(+1)] 5      == 12 (2*(5+1)= 12)
             apply [(3*),(4+),(5*)] 2 == 42 (3*(4+5*2) = 42)
             apply [] 5 == 5           
-}

apply :: [a -> a] -> a -> a
apply fs x = foldr (\f acc -> f acc) x fs


{- 
   Question 9 (3 marks):
   Use a fold operation (foldl or foldr) to implement a function 
   separate that takes a predicate p :: a -> Bool and a list l :: [a] 
   and returns a pair of list where the first list contains all 
   elements from l where p is True in the same order as l, and the 
   second list contains all elements from l where p is False in the same
   order as l. 
   Examples: separate even [1,2,3,4] == ([2,4],[1,3])
             separate odd [1,2,3,4]  == ([1,3],[2,4])
-}

separate :: (a -> Bool) -> [a] -> ([a],[a])
separate p = foldr(\x (fs, ss) -> if p x then (x:fs, ss) else (fs, x:ss)) ([], [])
