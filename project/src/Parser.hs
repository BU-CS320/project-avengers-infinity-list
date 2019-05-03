module Parser where

import Ast
import ParserMonad
import EnvUnsafeLog

-- | parser for the language
parser :: Parser Ast
parser = apps

keywords = ["if","then","else", "let", "in", "true","false"]

apps :: Parser Ast
apps = withInfix seps [("",App)] -- the tokens eat up all the spaces so we split on the empty string

seps :: Parser Ast
seps = seps' <|> cons

--right associative
seps' :: Parser Ast
seps' = do x <- (token cons)
           token (literal ";")
           y <- (token seps)
           return (Separator x y)

cons :: Parser Ast
cons = cons'' <|> cons' <|> orExpr

--right associative
cons' :: Parser Ast
cons' = do x <- (token orExpr)
           token (literal ":")
           y <- (token cons) <|> nil
           case y of (Cons _ _) -> return (Cons x y)
                     Nil        -> return (Cons x y)
                     _          -> return (Cons x (Cons y Nil))

cons'' :: Parser Ast
cons'' = do token $ literal "["
            x <- (token orExpr)
            token $ literal ","
            y <- (token cons)
            case y of (Cons _ _) -> return (Cons x y)
                      _          -> return (Cons x (Cons y Nil))


orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

andExpr :: Parser Ast
andExpr = withInfix comparison [("&&", And)]

comparison :: Parser Ast
comparison = withInfix addSubExpr [("==", Equals), ("/=", NotEquals), ("<=", LessThanOrEquals), ("<", LessThan), (">=" , GreaterThanOrEquals), (">", GreaterThan)]

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus),("-", Minus)]

multDivExpr :: Parser Ast
multDivExpr = withInfix expExpr [("*", Mult), ("/", Div)]

expExpr :: Parser Ast
expExpr = withInfix negExp' [("**", IntOrFloatExp)]

negExp :: Parser Ast
negExp = do token (literal "-")
            x <- (token listIndexExpr)
            return (NegExp x)

negExp' :: Parser Ast
negExp' = negExp <|> listIndexExpr

listIndexExpr :: Parser Ast
listIndexExpr = withInfix prefixExpr [("!!", ListIndex)]

prefixExpr :: Parser Ast
prefixExpr = notExp <|> printExp <|> atoms

printExp :: Parser Ast
printExp = do token (literal "print(")
              x <- parser
              token (literal ")")
              return (Print x)

atoms:: Parser Ast
atoms = floats <|> ints <|> chars <|> strings <|> bools  <|>  nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

notExp :: Parser Ast
notExp = do token (literal "!")
            x <- (token prefixExpr)
            return (Not x)

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do s <- token $ intParser
          return (ValInt s)

floats :: Parser Ast
floats = do s <- token $ floatParser
            return (ValFloat s)

chars :: Parser Ast
chars = do token $ literal "'"
           s <- token $ item
           token $ literal "'"
           return $ (ValChar s)

strings :: Parser Ast
strings = do token $ literal "\""
             s <- token $ varParser
             token $ literal "\""
             return $ (ValString s)

bools :: Parser Ast
bools = do s <- token $ varParser
           if (s == "true" || s == "false")
           then case s of
             "true" -> return (ValBool True)
             "false" -> return (ValBool False)
             otherwise -> failParse --should not happen
           else failParse


nil :: Parser Ast
nil = do token $ literal "[]"
         return Nil

ifParser :: Parser Ast
ifParser = do token $ literal "if"
              condition <- parser
              token $ literal "then"
              case1 <- parser
              token $ literal "else"
              case2 <- parser
              return (If condition case1 case2)


letParser :: Parser Ast
letParser = do token $ literal "let"
               label <- varParser
               token $ literal "="
               assigned <- parser
               token $ literal "in"
               ast <- parser
               return (Let label assigned ast)


lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  boundVar <- varParser
                  token $ literal "->"
                  ast <- parser
                  return $ Lam boundVar ast

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast

