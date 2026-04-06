import COSC4P41
import Data.List
import MyRandom

type HiddenWord   = String
type GuessesLeft  = Int
type GuessedChars = [Char]
type UserMove     = Char

newtype State     = State (HiddenWord,GuessesLeft,GuessedChars)

instance Show State where 
    show (State (hw,n,chars)) = "\n" ++ replicate 10 ' ' ++ intersperse ' ' (map (\c -> if c `elem` chars then c else '_') hw) ++ "\n\n" ++
                                "You have "++ show n ++ " moves left.\n" ++ "You have guessed the letters \"" ++ chars ++ "\" so far.\n"

gameStep :: State -> UserMove -> Either String (String,State)
gameStep state@(State (hw,n,chars)) c 
  | c `elem` chars      = Right ("You already tried that letter.",state)
  | subSet hw (c:chars) = Left "Congratulations!!! You won.\n\n"
  | c `elem` hw         = Right ("Good guess.",State (hw,n,c:chars))
  | n == 1              = Left "This letter is not in the word.\n You run out of moves.\n\n"
  | otherwise           = Right ("This letter is not in the word.",State (hw,n-1,c:chars))

iterateGame :: String -> State -> IO Bool
iterateGame message state = 
  do putStrLn message
     print state
     putStr "Guess a letter: "
     str0 <- getLine
     let str1 = trim str0
     if str1 == "quit" || str1 == "Quit" 
     then do putStrLn "Good bye."
             return True
     else if length str1 /= 1 
          then iterateGame ("Illegal input: " ++ str1) state
          else either (\s -> putStrLn s >> return False) (uncurry iterateGame) (gameStep state (head str1))
                                  
runGame :: [String] -> RandGen -> IO ()                  
runGame hiddenWords gen = do let (n,gen0) = randomInRange (0,length hiddenWords) gen
                             b <- iterateGame "*** Hangman Game ***\nType \"quit\" or \"Quit\" to quit.\n" (State (hiddenWords!!n,5,[]))
                             if b then return () else runGame hiddenWords gen0
         
main :: IO ()         
main = do contents <- readFile "HiddenWords.txt"
          let hiddenWords = split ',' contents
          gen <- getRandGen
          runGame hiddenWords gen