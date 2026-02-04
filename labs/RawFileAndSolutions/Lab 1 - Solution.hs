map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x:filter' p xs else filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x:xs) 
  | p x       = x:filter'' p xs 
  | otherwise = filter'' p xs


---------------------------------------

type Person = String
type Book   = String

type Database = [(Person,Book)]

exampleBase :: Database
exampleBase = [("Alice","Tintin"),("Anna","Little Women"),("Alice","Asterix"),("Rory","Tintin")]

books       :: Database -> Person -> [Book]
borrowers   :: Database -> Book -> [Person]
borrowed    :: Database -> Book -> Bool
numBorrowed :: Database -> Person -> Int
makeLoan    :: Database -> Person -> Book -> Database
returnLoan  :: Database -> Person -> Book -> Database

books dBase findPerson = [ book | (person,book) <- dBase , person==findPerson ]
borrowers dBase findBook = [ person | (person,book) <- dBase , book==findBook ]
borrowed dBase findBook = not (null (borrowers dBase findBook))
numBorrowed dBase findBook = length (borrowers dBase findBook)
-- borrowed dBase = not. null . borrowers dBase
-- numBorrowed dBase = length . borrowers dBase
makeLoan dBase person book = (person,book):dBase
returnLoan dBase person book = filter ((person,book)/=) dBase


