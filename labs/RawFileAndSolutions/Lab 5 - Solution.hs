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

instance Show Ops where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"  

instance Show Expr where
  showsPrec _ (Lit n)           = shows n
  showsPrec _ (Var v)           = showString v
  showsPrec n (Op op e1 e2)     = showParen (n > p) $ (showsPrec p e1) . shows op . (showsPrec p e2)
    where p = prec op

type Store = Var -> Integer

initial :: Store 
initial = const 0

value   :: Store -> Var -> Integer 
value = ($)

update  :: Store -> Var -> Integer -> Store 
update st v n = \w -> if v == w then n else st w

semOps :: Ops -> (Integer -> Integer -> Integer)
semOps Add = (+)
semOps Sub = (-)
semOps Mul = (*)
semOps Div = div
semOps Mod = mod

eval :: Expr -> Store -> Integer
eval (Lit n)       _  = n
eval (Var v)       st = value st v
eval (Op op e1 e2) st = semOps op (eval e1 st) (eval e2 st)

data Command = Eval Expr | Assign Var Expr | Null deriving Show

command :: Command -> Store -> (Integer,Store) 
command Null         st = (0 , st) 
command (Eval e)     st = (eval e st , st) 
command (Assign v e) st = (val , newSt) 
   where val   = eval e st 
         newSt = update st v val
         
languageDef :: LanguageDef ()
languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = ["Store","Eval","Null","."]
           , Token.reservedOpNames = ["+","-","*","/","%",":="]
           }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer 

reserved :: String -> Parser ()
reserved   = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer 

parens :: Parser a -> Parser a
parens     = Token.parens lexer 

integer :: Parser Integer
integer    = Token.integer lexer 

expression :: Parser Expr
expression = buildExpressionParser operators term
 
operators :: OperatorTable Char () Expr
operators = [ [Infix  (reservedOp "*"   >> return (Op Mul)) AssocLeft,
                Infix  (reservedOp "/"   >> return (Op Div)) AssocLeft,
                Infix  (reservedOp "%" >> return (Op Mod)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Op Add)) AssocLeft,
                Infix  (reservedOp "-"   >> return (Op Sub)) AssocLeft]
              ]

term :: Parser Expr
term =  parens expression
     <|> do
            var <- identifier
            return $ Var var
     <|> do
            n <- integer
            return $ Lit n
            
commParser :: Parser Command
commParser =   do reserved "Store"
                  var <- identifier
                  reservedOp ":="
                  expr <- expression
                  reserved "."
                  return $ Assign var expr
             <|>
               do reserved "Eval"
                  expr <- expression
                  reserved "."
                  return $ Eval expr
             <|> 
               do reserved "Null"
                  return Null
             
commLine :: String -> IO Command
commLine str =
  case parse commParser "" str of
    Left e  -> print e >> return Null
    Right r -> return r  
    
calcStep :: Store -> IO Store 
calcStep st = do line <- getLine 
                 comm <- commLine line
                 let (val , newSt) = command comm st
                 print val
                 return newSt 

calcSteps :: Store -> IO () 
calcSteps st = do res <- isEOF 
                  if not res 
                  then do newSt <- calcStep st 
                          calcSteps newSt
                  else return ()
                  
main :: IO ()
main = calcSteps initial                  

