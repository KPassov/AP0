module CurvySyntax where

import Data.Char(isDigit)
import CurveAST
import SimpleParse

curveList :: Parser Curve
curveList = do schar '('
               c <- curve
               schar ')'
               return c 

curve :: Parser Curve
curve = (do cv <- term
            rest cv )
        where rest cv = connect cv
           
term :: Parser Curve
term = point >>= return . Single

expr :: Parser Expr
expr = add
      <|> width
      <|> height

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

point :: Parser Point
point = do schar '('
           exprl <- expr 
           schar ','
           exprr <- expr       
           schar ')'
           return (Point exprl exprr)

ident :: Parser String
ident = do iv <- idents
           return iv
           where idents = many1(satisfy (`elem` allowed)) 
                 allowed    = '_':['a'..'z']++['A'..'Z']++['0'..'9']
                 {- notallowed = ["where","refh","refv","rot","width","height"] -}

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
parseString input = parse (do exprv <- curve
                              token eof
                              return exprv) input 

{- parseFile :: FilePath -> IO (Either Error Program) -}
{- parseFile _ = undef -}
