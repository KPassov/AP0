module CurvySyntax where

import CurveAST

data Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p



instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat  [parse (f a) cs' |

    (<++) :: Parser a -> Parser a -> Parser a
    p <++ q = Parser (\cs -> case parse p cs of 
                                    [] -> parse q cs   
                                    res <- res)

class Monad m => MonadZero m where
      zero :: m a
 
class MonadZero m => MonadPlus m where
      (++) :: m a -> m a -> m a

instance MonadZero Parser where
      zero = Parser (\cs -> [])

item :: Parser Char
item =  Parser (\cs -> case cs of
                       ""     -> []
                       (c:cs') -> [(c,cs')])



undef :: t
undef = undef
data Error = String | Nothing 

parseString :: String -> Either Error Program
parseString _ = undef

parseFile :: FilePath -> IO (Either Error Program)
parseFile _ = undef
