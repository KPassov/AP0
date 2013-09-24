module CurvySyntax (parseString) where

import Data.Char(isDigit)
import CurveAST
import SimpleParse

program :: Parser [Def]
program = defs

defs :: Parser [Def]
defs = do d <- many1 def
          return d
             
def :: Parser Def
def = do iv <- ident
         schar '='
         cv <- curve
         symbol "where"
         schar '{'
         dsv <- many1 def
         schar '}'
         return (Def iv cv dsv)
  <|> do iv1 <- ident
         schar '='
         cv1 <- curve
         return (Def iv1 cv1 [])
  
  
curve :: Parser Curve
curve = connect
      
           
term :: Parser Curve
term = curvePres
     <|> do pv <- point
            po <- translate(Single pv)
            return po
    <|> do iv <- ident
           io <- translate(Id iv)
           return io
    
connect :: Parser Curve
connect = over `chainl1` (symbol "++" >> return Connect)

over :: Parser Curve
over = term `chainl1` (schar '^' >> return Over)

curvePres :: Parser Curve
curvePres = do schar '('
               c <- curve
               schar ')'
               io <- translate(c)
               return io


translate :: Curve -> Parser Curve
translate cv = do symbol "->"
                  p <- point
                  translate (Translate cv p)
              <|> scale cv 

scale :: Curve -> Parser Curve
scale cv = do symbol "**"
              exprr <- expr
              translate (Scale cv exprr)
          <|> refv cv 


refv :: Curve -> Parser Curve
refv cv = do symbol "refv"
             exprr <- expr
             translate (Refv cv exprr)
         <|> refh cv 

refh :: Curve -> Parser Curve
refh cv = do symbol "refh"
             exprr <- expr
             translate (Refh cv exprr)
         <|> rot cv 

rot :: Curve -> Parser Curve
rot cv = do symbol "rot"
            exprr <- expr
            translate (Rot cv exprr)
        <|> return cv 


expr :: Parser Expr
expr = add

add    :: Parser Expr
add    = mult   `chainl1` (schar '+' >> return Add)

mult   :: Parser Expr
mult   = exprTerm `chainl1` (schar '*' >> return Mult)


exprTerm :: Parser Expr
exprTerm = number
          <|> width
          <|> height
          <|> exprPar

width  :: Parser Expr
width  = do symbol "width" 
            c <- curve
            return (Width c)

height :: Parser Expr
height = do symbol "height" 
            c <- curve
            return (Height c)
            
exprPar :: Parser Expr
exprPar = do schar '('
             e <- expr
             schar ')'
             return e 

point :: Parser Point
point = do schar '('
           exprl <- expr 
           schar ','
           exprr <- expr       
           schar ')'
           return (Point exprl exprr)

ident :: Parser Ident
ident = token (do iv <- idents
                  if checkIdents iv
                    then reject
                    else return iv)
                where idents = many1(satisfy (`elem` allowed))
                      checkIdents s = s `elem` notallowed
                      allowed = '_':['a'..'z']++['A'..'Z']++['0'..'9']
                      notallowed = ["where", "refv", "refh", "rot", "width","height"]
                      

number :: Parser Expr
number = token (do pre <- digits
                   rest pre) 
         where digits = many1(satisfy isDigit)  
               rest pre = do schar '.'
                             post <- digits
                             return $ Const $ read $ pre ++ "." ++ post
                         <|> do return $ Const $ read $ pre 

{- type Error = String -}
{- parseString :: String -> Either Error Program -}
parseString input = 
  case parse (do {e <- program; token eof; return e}) input of 
	  [(e, [])] -> Right e
	  _			-> Left "parse error"

{- parseFile :: FilePath -> IO (Either Error Program) -}
{- parseFile _ = undef -}

