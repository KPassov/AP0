
import Text.ParserCombinators.ReadP
import Data.Char(isSpace,isDigit)
import qualified Data.Map as M

data SExp = IntVal Int
          | SymbolVal String
          | ListExp [SExp]
            deriving (Show, Eq, Ord)

type Parser a = ReadP a

(<|>) :: ReadP a -> ReadP a -> ReadP a
(<|>) = (+++)
parse :: ReadP a -> ReadS a
parse = readP_to_S

token           :: Parser a -> Parser a
token p          = skipSpaces >> p


symbol :: String -> Parser String
symbol           = token . string

schar :: Char -> Parser Char
schar            = token . char

numberOrSymbol :: Parser SExp
numberOrSymbol = token $ do s <- munch1 $ \c -> not(isSpace c || c `elem` "()")
                            return $ if all isDigit s then IntVal $ read s
                                     else SymbolVal s
sexp :: Parser SExp
sexp = numberOrSymbol
       <|> between (schar '(') (schar ')') sexps
  where sexps = many sexp >>= return . ListExp

parseString :: String -> Either String SExp
parseString s =
  case parse (do {e <- sexp; token eof; return e}) s of
      [(e, [])] -> Right e
      _         -> Left "Parse error"


true :: SExp -> Bool
true (ListExp []) = False
true _ = True

type Environment = M.Map String SExp 

lookupVar :: String -> Environment -> Maybe SExp
lookupVar = M.lookup

bindvar :: String -> SExp -> Environment -> Environment
bindvar = M.insert 

getVars :: Environment -> [SExp]
getVars = M.elems

emptyEnv :: Environment
emptyEnv = M.empty

newtype APLispExec a = RC { runLisp :: Environment -> Maybe a }

local :: (Environment -> Environment) -> APLispExec a -> APLispExec a
local f m = RC $ \env -> let env' = f env
                         in runLisp m env'

{- data SExp = IntVal Int -}
          {- | SymbolVal String -}
          {- | ListExp [SExp] -}

eval :: SExp -> APLispExec SExp
eval (SymbolVal s) = lookupVar s
eval (IntVal v) = return $ IntVal v
eval (ListExp []) = return $ ListExp []
eval (ListExp [SymbolVal "quote", x]) = return x
eval (ListExp (SymbolVal "quote":_)) = fail "Quote called with wrong number of arguments"
eval (ListExp (SymbolVal "if",c,texp,fexp)) = return $ ifexp 
                                where ifexp = if c then texp else fexp 
eval (ListExp (SymbolVal "if":_)) = fail "If called with wrong number of arguments"
{- eval (ListExp (SymbolVal "let" )) = fail "If called with wrong number of arguments" -}
  -- Call apply with (SymbolVal s) and the result of evaluating args.

apply :: SExp -> [SExp] -> APLispExec SExp
apply (IntVal _) _ = fail "Cannot apply integers!"
apply (ListExp _) _ = fail "Cannot apply lists!"
apply (SymbolVal "list") xs = return $ ListExp xs


