import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.IO

data Expr = Lit Integer | Var Var | Op Ops Expr Expr
data Ops = Add | Sub | Mul | Div | Mod
type Var = String 

prec :: Ops -> Int


-- Make Ops printable 


-- Make Expr printable 

type Store = Var -> Integer

initial :: Store 
initial = const 0

value   :: Store -> Var -> Integer 


update  :: Store -> Var -> Integer -> Store 


semOps :: Ops -> (Integer -> Integer -> Integer)



eval :: Expr -> Store -> Integer



data Command = Eval Expr | Assign Var Expr | Null deriving Show

command :: Command -> Store -> (Integer,Store) 

         
languageDef :: LanguageDef ()


lexer :: Token.TokenParser ()



identifier :: Parser String


reserved :: String -> Parser ()


reservedOp :: String -> Parser ()



parens :: Parser a -> Parser a


integer :: Parser Integer


expression :: Parser Expr

 
operators :: OperatorTable Char () Expr


term :: Parser Expr

 
            
commParser :: Parser Command


             
commLine :: String -> IO Command
 
    
calcStep :: Store -> IO Store 


calcSteps :: Store -> IO () 


                  
main :: IO ()
                

