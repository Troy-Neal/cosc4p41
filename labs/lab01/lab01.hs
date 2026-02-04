calculateAreaRectangle:: Double -> Double -> Double
calculateAreaRectangle length width = length * width
calculateAreaSquare l = l^2
calculateAreaSquare' l = calculateAreaRectangle l l

factorial:: Integer -> Integer
factorial 0 = 1
factorial n = factorial(n-1) * n


type Person = String
type Book = String

type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice","Tintin"),("Anna","Little Women"), ("Alice","Asterix"), ("Rory","Tintin")]
-- Add a book loan to the database
makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase person book = (person, book): dBase


-- Get All Books that a given person has a hold On
books:: Database -> Person -> [Book]
books dBase person = [n | (m,n) <- dBase, m == person]


booksRec:: Database -> Person -> [Book]
booksRec [] _ = []
booksRec ((p, b):dBase) person
 | person == p = b : books dBase person
 | otherwise = books dBase person
 
borrowed:: Database -> Book -> Bool
borrowed [] _ = False
borrowed ((p,b):dBase) book
 | b == book = True
 | otherwise = borrowed dBase book
 
borrowed':: Database -> Book -> Bool
borrowed' dBase book = length [b | (p,b) <- dBase, b == book] > 0
 
 
numBorrowed :: Database -> Person -> Int
numBorrowed [] _ = 0
numBorrowed ((p,b):dBase) person
 | p == person = 1 + numBorrowed dBase person
 | otherwise = 0 + numBorrowed dBase person
 
 
numBorrowed' :: Database -> Person -> Int
numBorrowed' dBase person = length (books dBase person)   

flt:: (a -> Bool) -> [a] -> [a]
flt _ [] = []
flt p (x:xs)
 | p x == True = x : flt p xs
 | otherwise = flt p xs
 
odds :: [Int]
odds = flt (\x -> odd x) [200 .. 500] 