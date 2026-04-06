import Data.Ratio
      
-- computes the list of dividers from 1 to sqr |n| for the absolute value |n| with n <> 0     
basic_dividers :: Integer -> [Integer]          
basic_dividers n = [ x | x <- [ 1..(floor . sqrt . fromInteger . abs) n ], n `mod` x == 0 ]

-- computes the list of positive and negative dividers of an integer n <> 0
dividers :: Integer -> [Integer]
dividers n = concatMap (\x -> if abs (n `div` x) == abs x then [x,-x] else [x,-x,n `div` x,-(n `div` x)]) (basic_dividers n)

-- a normalized integer polynomial represented by its list of coefficients starting from 
-- the lowest exponent and admitting the coefficient 1 for the highest exponent, i.e.,
-- x^2-4x-5 is represented by [-5,-4]
type NormIntPolynomial = [Integer]

-- computes the value of a normalized integer polynomial for a given value using the
-- representation 
-- a_0+a_1*x+a_2*x^2+...+a_{n-1}x^(n-1)+x^n=a_0+x*(a_1+x*(a_2+x*(...(a_{n-1}+1*x)..)))
valueNIP :: NormIntPolynomial -> Integer -> Integer
valueNIP p x = foldr (\y z -> y + x*z) 1 p

-- computes the integer roots of a normalized integer polynomial
rootsNIP :: NormIntPolynomial -> [Integer]
rootsNIP p@(a0:pl) | a0 == 0   = 0:rootsNIP pl 
                   | otherwise = filter ((==0) . valueNIP p) (dividers a0)

-- type of general integer polynomials
type IntPolynomial = [Integer]

-- converts an integer polynomial p to a normalized integer polynomial q so that
-- so that the rational roots of p are the integer roots of q divided by a_n
toNIP :: IntPolynomial -> NormIntPolynomial
toNIP p = zipWith (\x y -> x*(last p)^y) p [length p -2,length p -3..0]

-- computes the rational roots of an integer polynomial by using toNIP
rootsIP :: IntPolynomial -> [Rational]
rootsIP p = map (% last p) (rootsNIP $ toNIP p)

-- type of rational polynomials
type RationalPolynomial = [Rational]

-- converts a rational polynomial into an integer polynomial with the same
-- roots by multiplying the polynomial by the least common multiplier of all
-- denominators.
toIP :: RationalPolynomial -> IntPolynomial
toIP p = map (numerator . (fromInteger (foldl1 lcm (map denominator p))*)) p

-- computes the rational roots of a rational polynomial by using toIP    
rootsRP :: RationalPolynomial -> [Rational]
rootsRP = rootsIP . toIP

-- Examples
nip = [ 8*12^3, -2*12^2, -29*12, -4] 
ip  = [8,-2,-29,-4,12]
rp  = [4 % 3,-1 % 3,-29 % 6,-2 % 3,2 % 1]