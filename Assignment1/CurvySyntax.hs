module CurvySyntax where

import Data.Char(isDigit)
import CurveAST
import SimpleParse

add    = mult   `chainl1` (char '+' >> return Add)
mult   = number `chainl1` (char '*' >> return Add)

{- width  = curve `chainl1` (char '+' >> return Add) -}
{- height = curve `chainl1` (char '+' >> return Add) -}

{- expr :: Parser Expr -}
{- expr = (do   -}
           {- expv1 <- expr -}
           {- expv2 <- expr -}
           {- return expv1 )  -}
   {- <|> (do num <- number -}
           {- return num) -}
  {- where plusop = (do symbol "+" -}
                     {- return Add) -}

number :: Parser Number
number = token (do pre <- digits
                   char '.'
                   post <- digits
                   return $ read $ pre ++ "." ++ post)
         where digits = many1(satisfy isDigit)  

type Error = String
{- parseString :: String -> Either Error Program -}
parseString input = parse (do exprv <- expr
                              token eof
                              return exprv) input 

{- parseFile :: FilePath -> IO (Either Error Program) -}
{- parseFile _ = undef -}
