-- List of all powers of 2 from 1 to 2000
--powers :: [Integers]
--powers = map(\x -> 2^x)[1...2000]

map'::(a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
