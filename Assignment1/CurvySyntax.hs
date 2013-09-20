module CurvySyntax where

import Data.Char(isDigit)
import CurveAST
import SimpleParse
import Control.Monad(liftM) 

program :: Parser [Def]
program = defs

defs :: Parser [Def]
defs = do d <- def
          ds <- defs
          return (d:ds)
   <|> do dv <- def
          return [dv]
   
def :: Parser Def
def = do iv <- ident
         schar '='
         cv <- curve
         symbol "where"
         schar '{'
         dsv <- defs
         schar '}'
         return (Def iv cv dsv)
  <|> do iv1 <- ident
         schar '='
         cv1 <- curve
         return (Def iv1 cv1 [])
  
  
curve :: Parser Curve
curve = do cv <- term
           rest cv 
        where rest = connect
           
term :: Parser Curve
term = curvePres
     <|> liftM Single point
     <|> liftM Id ident 

curvePres :: Parser Curve
curvePres = do schar '('
               c <- curve
               schar ')'
               return c 


connect :: Curve -> Parser Curve
connect cv = do symbol "++" 
                ch <- curve
                connect (Connect cv ch) 
            <|> over cv

over :: Curve -> Parser Curve
over cv = do schar '^' 
             ch <- curve 
             connect (Over cv ch) 
         <|> translate cv

translate :: Curve -> Parser Curve
translate cv = do symbol "->"
                  p <- point
                  connect (Translate cv p)
              <|> scale cv 

scale :: Curve -> Parser Curve
scale cv = do symbol "**"
              exprr <- expr
              connect (Scale cv exprr)
          <|> refv cv 


refv :: Curve -> Parser Curve
refv cv = do symbol "refv"
             exprr <- expr
             connect (Refv cv exprr)
         <|> refh cv 

refh :: Curve -> Parser Curve
refh cv = do symbol "refh"
             exprr <- expr
             connect (Refh cv exprr)
         <|> rot cv 

rot :: Curve -> Parser Curve
rot cv = do symbol "rot"
            exprr <- expr
            connect (Rot cv exprr)
        <|> return cv 


expr :: Parser Expr
expr = add
      <|> width
      <|> height
      <|> exprPar

add    :: Parser Expr
add    = mult   `chainl1` (schar '+' >> return Add)

mult   :: Parser Expr
mult   = number `chainl1` (schar '*' >> return Mult)

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
ident = token idents 
                where idents = many1(satisfy (`elem` allowed))
                      allowed = '_':['a'..'z']++['A'..'Z']++['0'..'9']

number :: Parser Expr
number = token (do pre <- digits
                   rest pre) 
         where digits = many1(satisfy isDigit)  
               rest pre = do schar '.'
                             post <- digits
                             return $ Const $ read $ pre ++ "." ++ post
                         <|> do return $ Const $ read pre 

type Error = String
parseString :: String -> Either Error Program
parseString input = 
  case parse (do {e <- program; token eof; return e}) input of 
      [(e, [])] -> Right e
      _	        -> Left "parse error"

{- parseFile :: FilePath -> IO (Either Error Program) -}
{- parseFile _ = undef -}
