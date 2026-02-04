import Data.Ratio
import Data.List
      
-- computes the list of dividers from 1 to sqr |n| for the absolute value |n| with n <> 0     

basic_dividers :: Integer -> [Integer]          
basic_dividers n = [x | x <- [ 1 .. (floor.sqrt.fromIntegral.abs) n], n `mod` x == 0 ]

--                          Old Version
--basic_dividers :: Integer -> [Integer]          
--basic_dividers n = [x | x <- [ 1 ..  floor ( sqrt ( fromIntegral ( abs n ) ) )], mod n x == 0 ]

-- computes the list of positive and negative dividers of an integer n <> 0
dividers :: Integer -> [Integer]
dividers n = concatMap (\x -> if n `div` x == abs x then [-x,x] else [-x, x, n `div` x, -(n `div` x)]) (basic_dividers n)

-- a normalized integer polynomial represented by its list of coefficients starting from 
-- the lowest exponent and admitting the coefficient 1 for the highest exponent, i.e.,
-- x^2-4x-5 is represented by [-5,-4]
type NormIntPolynomial = [Integer]


-- computes the value of a normalized integer polynomial for a given value using the
-- representation 
-- a_0+a_1*x+a_2*x^2+...+a_{n-1}x^(n-1)+x^n=a_0+x*(a_1+x*(a_2+x*(...(a_{n-1}+1*x)..)))
valueNIP :: NormIntPolynomial -> Integer -> Integer
valueNIP nip x = foldr (\ce acc -> ce + x * acc ) 1 nip

-- computes the integer roots of a normalized integer polynomial
rootsNIP :: NormIntPolynomial -> [Integer]
rootsNIP orig@(a0:pl)
 | a0 == 0 = 0 : rootsNIP pl
 | otherwise = filter (\x -> valueNIP orig x == 0 ) (dividers a0) 

-- type of general integer polynomials
type IntPolynomial = [Integer]

-- converts an integer polynomial p to a normalized integer polynomial q so that
-- so that the rational roots of p are the integer roots of q divided by a_n
toNIP :: IntPolynomial -> NormIntPolynomial

sum' xs  = foldr (\x acc -> x + acc) 0 xs
sum'' xs = foldl (\acc x -> x + acc) 0 xs
