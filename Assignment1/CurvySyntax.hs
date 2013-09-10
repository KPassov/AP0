module CurvySyntax where

import CurveAST

data Parser a = Parser (String -> [(a,String)])

data Error = String | Nothing 

item :: Parser Char
item =  Parser (\cs -> case cs of
                       ""     -> []
                       (c:cs') -> [(c,cs')])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat  [parse (f a) cs' |
                                      (a,cs') <- parse p cs])
class Monad m => MonadZero m where
      zero :: m a
 
class MonadZero m => MonadPlus m where
      (++) :: m a -> m a -> m a

instance MonadZero Parser where
      zero = Parser (\cs -> [])

instance MonadPlus Parser where
      p ++ q = Parser (\cs -> parse p cs ++ parse q cs)   



undef :: t
undef = undef

parseString :: String -> Either Error Program
parseString _ = undef

parseFile :: FilePath -> IO (Either Error Program)
parseFile _ = undef
