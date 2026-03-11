import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.IO

data Expr = Lit Integer | Var Var | Op Ops Expr Expr
data Ops = Add | Sub | Mul | Div | Mod 
type Var = String 


prec :: Ops -> Int
prec Add = 3
prec Sub = 3
prec Mul = 6
prec Div = 6
prec Mod = 6

-- Make Ops printable 

instance Show (Ops) where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"


-- Make Expr printable 
instance Show (Expr) where
  showsPrec _ (Lit n) = shows n
  showsPrec _ (Var v) = shows v
  showsPrec p (Op operator le re) = showParen(p > pres) (showsPrec pres le).shows operator.(showsPrec pres re)
    where pres = prec operator

type Store = Var -> Integer

initial :: Store 
initial = const 0

value :: Store -> Var -> Integer
value = ($)

update  :: Store -> Var -> Integer -> Store
update st var val = \w -> if w == var then val else st w

secondStore =  (update (update (update initial "w" 8) "y" 10)"z" 16)

semOps :: Ops -> (Integer -> Integer -> Integer)
semOps Add = (+)
semOps Sub = (-)
semOps Div = (div)
semOps Mod = (mod)
semOps Mul = (*)

eval :: Expr -> Store -> Integer
eval (Lit n) st = n
eval (Var v) st = st v
eval (Op operator le re) st = semOps operator (eval le st) (eval re st)

data Command = Eval Expr | Assign Var Expr | Null deriving Show

command :: Command -> Store -> (Integer,Store)
command Null st = (0, st)
command (Eval expr) st = (eval expr st, st)
command (Assign var expr) st = (result, newStore)
  where result = eval expr st
        newStore = update st var result

languageDef :: LanguageDef ()
languageDef = emptyDef {
                Token.identStart = letter,
                Token.identLetter = alphaNum,
                Token.reservedNames = ["Store", "Null", "Eval", "."],
                Token.reservedOpNames = ["+", "-", "*", "/", "%", ":="]
              }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

{-
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
                
-}