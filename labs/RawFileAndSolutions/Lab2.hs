import Data.Ratio
      
-- computes the list of dividers from 1 to sqr |n| for the absolute value |n| with n <> 0     
basic_dividers :: Integer -> [Integer]          

-- computes the list of positive and negative dividers of an integer n <> 0
dividers :: Integer -> [Integer]

-- a normalized integer polynomial represented by its list of coefficients starting from 
-- the lowest exponent and admitting the coefficient 1 for the highest exponent, i.e.,
-- x^2-4x-5 is represented by [-5,-4]
type NormIntPolynomial = [Integer]

-- computes the value of a normalized integer polynomial for a given value using the
-- representation 
-- a_0+a_1*x+a_2*x^2+...+a_{n-1}x^(n-1)+x^n=a_0+x*(a_1+x*(a_2+x*(...(a_{n-1}+1*x)..)))
valueNIP :: NormIntPolynomial -> Integer -> Integer


-- computes the integer roots of a normalized integer polynomial
rootsNIP :: NormIntPolynomial -> [Integer]

-- type of general integer polynomials
type IntPolynomial = [Integer]

-- converts an integer polynomial p to a normalized integer polynomial q so that
-- so that the rational roots of p are the integer roots of q divided by a_n
toNIP :: IntPolynomial -> NormIntPolynomial


-- computes the rational roots of an integer polynomial by using toNIP
rootsIP :: IntPolynomial -> [Rational]


-- type of rational polynomials
type RationalPolynomial = [Rational]

-- converts a rational polynomial into an integer polynomial with the same
-- roots by multiplying the polynomial by the least common multiplier of all
-- denominators.
toIP :: RationalPolynomial -> IntPolynomial

-- computes the rational roots of a rational polynomial by using toIP    
rootsRP :: RationalPolynomial -> [Rational]


-- Examples
nip = [ 8*12^3, -2*12^2, -29*12, -4] 
ip  = [8,-2,-29,-4,12]
rp  = [4 % 3,-1 % 3,-29 % 6,-2 % 3,2 % 1]