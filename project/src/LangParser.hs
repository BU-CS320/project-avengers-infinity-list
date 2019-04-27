module LangParser where

import Lang
import ParserMonad
import EnvUnsafe



parser :: Parser Ast
parser = apps

-- note that the parser must respect all the precedence and associativity rules expressed in the prettyShow function.
-- that means
-- ! binds more tightly than
-- * / which binds more tightly than
-- + - which binds more tightly than
-- && which binds more tightly than
-- || which binds more tightly than
-- : which binds more tightly than
-- {the application} which binds weakest of all

-- + - * / && || {the application} are left associative
-- : is right associative

-- we are mostly following the questionable c precedence rules

-- ungraded bonus: add additional pretty syntax for lists: [1,2,3,4]

-- you may want to structure you grammar like this:

keywords = ["if","then","else", "let", "in", "true","false"]

apps :: Parser Ast
apps = withInfix cons [("",App)] -- the tokens eat up all the spaces so we split on the empty string

cons :: Parser Ast
cons = cons' <|> orExpr

--right associative
cons' :: Parser Ast
cons' = do x <- (token orExpr)
           token (literal ":")
           y <- (token cons)
           case y of (Cons _ _) -> return (Cons x y)
                     _          -> return (Cons x (Cons y Nil))


-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")


orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

-- *LangParser> parse orExpr "true || false && 7"
-- Just (true || false && 7,"")
-- *LangParser> parse orExpr "true || false || 7"
-- Just (true || false || 7,"")
-- *LangParser> parse orExpr "true"
-- Just (true,"")

andExpr :: Parser Ast
andExpr = withInfix addSubExpr [("&&", And)]

-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("-", Minus)]

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDivExpr :: Parser Ast
multDivExpr = withInfix expExpr [("*", Mult), ("/", Div)]

expExpr :: Parser Ast
expExpr = withInfix listIndexExpr [("**", IntExp)]

listIndexExpr :: Parser Ast
listIndexExpr = withInfix notExp [("!!", ListIndex)]

notExp :: Parser Ast --HAS PROBLEMS
notExp = notExp' <|> atoms

notExp' :: Parser Ast
notExp' = do token (literal "!")
             x <- (token notExp)
             return (Not x)

atoms:: Parser Ast
atoms = ints <|> bools  <|>  nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

-- *LangParser> parse atoms "111"
-- Just (111,"")
-- *LangParser> parse atoms "  true"
-- Just (true,"")

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do s <- token $ intParser
          return (ValInt s)

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

-- *LangParser> parse letParser "let x=3 in x+x"
-- Just (let x = 3 in x + x,"")


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

-- *LangParser> parse parser "(true)"
-- Just (true,"")
-- *LangParser> parse parser "let x = (if true and false then 3 else elsee) in x + x"
-- Just (let x = if true and false then 3 else elsee in x + x,"")




-- Some examples of weird stuff

ex = showPretty  (Mult (Var "a") (Or (Var "b") (Var "c"))) 0

ex1 = showPretty (Minus (Var "y") (Minus (App (ValBool True) (ValInt (-3))) (Mult (ValBool False) (ValBool False)))) 0

ex2 = showPretty (Cons (Var "z") (Not (Not (Plus (Mult (ValInt (-18)) Nil) (Not (ValInt 2)))))) 0

ex3 = "! ! (-18)"

-- for repl testing
data LangOut = ParseError | RuntimeError String | Result Val deriving Show

exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast,"") -> case run ast of
                     Ok v -> Result v
                     Error e -> RuntimeError e
  _  -> ParseError


{- Note: The easiest thing is to simply copy this code into
   your LangParser.hs file. 

   To run the function test below, in Lang.hs you will
   need to derive Show for the Ast rather than
   make it an instance of Show and use prettyPrint, that is:

          ....

         | Lam String Ast
         | App Ast Ast
         deriving (Eq,Show) -- helpful to use this during testing
--         deriving Eq

--instance Show Ast where
--  show ast = showPretty ast 0

-}



-- Examples and Easy Test Cases

t1 = "5"

t2 = "thisIsAnIdentifier"

t3 = "true"

t4 = "false"

t5 = "[]"

t6 = "[   ]"

t7 = "! false"

t8 = "!5"

t9 = "! ! ! []"

t10 = "4 * 2"

t11 = "2 / 1"

t12 = "3 * 9 / value  * 4"

t13 = "!4 * true / ! ! []" --partial parse

t14 = "2 + 4"

t15 = "9 - 2"

t16 = "2 * x - 2 / 1"

t17 = "! 6 * [] - true"

t18 = "true && b "

t19 = "false || true"

t20 = "false && true || false && false"

t21 = "! false && ! boolIdent || ! true"

t22 = "4 && ![] || false"

t23 = " 4 : []"

t24 = "true : false : x : []" --partial parse

t25 = "4 + 2 : 5 / 1 : ! false && true || z : []" --partial parse

t26 = "f 5"

t27 = "f g h 3"

t28 = " f g 8 * 2"

t29 = "f 3 : []"

t30 = "if true then 2 else 6"

t31 = "if true && false then 2 + 5 else 9 : []"

t32 = "if true then 3 else if false then 9 else 1"

t33 = "let x = 5 in x"

t34 = "let x = 5 * 7 in x : []"

t35 = "let x = true : [] in let y = 5 in y : x"

t36 = "let y = if 3 then 2 else true in let y = if true then 5 else [] in let x = 4 in x"

t37 = "\\x -> 5"

t38 = "\\x -> \\y -> x"

t39 = "\\x -> let y = x in x && y"

t40 = "\\x -> if x then true else y : []"

t41 = "\\x -> x y"

t42 = "\\x -> x : y"

t43 = "if \\x -> x then \\y -> y else \\z -> z"

t44 = "let f = \\x -> x y in \\z -> f z x"

t45 = "\\x -> x + 4 : []"

t46 = "4 * (2 + 9)"

t47 = "f (\\x -> x y) ( (4 : 3) x ) "

t48 = "f (g x) (h 4)"

t49 = "3 + if true then 5 else 9"

t50 = "if true then 5 else 9 + 3"

t51 = "(if true then 5 else 9) + 3"

t52 = "2 : \\x -> x : 1"

t53 = "8 - let x = 2 in 1 - 3"

-- These are the "weird" examples from the distribution file

t54 = "a * (b || c)"

t55 = "y - ((true (-3)) - false * false)"

t56 = "z :  !  ! ((-18) * [] +  ! 2)" --partial parse

t57 =  "! ! (-18)" --no parse

-- Just some code to help testing

p = parse parser

p' x = case parse parser x of
          Just (res,"") -> showPretty res 0
          Just (res,_)  -> "Partial parse: " ++ showPretty res 0 
          Nothing       -> "Parsing Error"

test x = do print x
            putStrLn $ show $ p x
            putStrLn $ p' x
            putStrLn ""

{-

LangParser> test t1
"5"
Just (ValInt 5,"")
5

LangParser> test t2
"thisIsAnIdentifier"
Just (Var "thisIsAnIdentifier","")
thisIsAnIdentifier

LangParser> test t3
"true"
Just (ValBool True,"")
true

LangParser> test t4
"false"
Just (ValBool False,"")
false

LangParser> test t5
"[]"
Just (Nil,"")
[]

LangParser> test t6
"[   ]"
Just (Nil,"")
[]

LangParser> test t7
"! false"
Just (Not (ValBool False),"")
 ! false

LangParser> test t8
"!5"
Just (Not (ValInt 5),"")
 ! 5

LangParser> test t9
"! ! ! []"
Just (Not (Not (Not Nil)),"")
 !  !  ! []

LangParser> test t10
"4 * 2"
Just (Mult (ValInt 4) (ValInt 2),"")
4 * 2

LangParser> test t11
"2 / 1"
Just (Div (ValInt 2) (ValInt 1),"")
2 / 1

LangParser> test t12
"3 * 9 / value  * 4"
Just (Mult (Div (Mult (ValInt 3) (ValInt 9)) (Var "value")) (ValInt 4),"")
3 * 9 / value * 4

LangParser> test t13
"!4 * true / ! ! []"
Just (Div (Mult (Not (ValInt 4)) (ValBool True)) (Not (Not Nil)),"")
 ! 4 * true /  !  ! []

LangParser> test t14
"2 + 4"
Just (Plus (ValInt 2) (ValInt 4),"")
2 + 4

LangParser> test t15
"9 - 2"
Just (Minus (ValInt 9) (ValInt 2),"")
9 - 2

LangParser> test t16
"2 * x - 2 / 1"
Just (Minus (Mult (ValInt 2) (Var "x")) (Div (ValInt 2) (ValInt 1)),"")
2 * x - 2 / 1

LangParser> test t17
"! 6 * [] - true"
Just (Minus (Mult (Not (ValInt 6)) Nil) (ValBool True),"")
 ! 6 * [] - true

LangParser> test t18
"true && b "
Just (And (ValBool True) (Var "b"),"")
true && b

LangParser> test t19
"false || true"
Just (Or (ValBool False) (ValBool True),"")
false || true

LangParser> test t20
"false && true || false && false"
Just (Or (And (ValBool False) (ValBool True)) (And (ValBool False) (ValBool False)),"")
false && true || false && false

LangParser> test t21
"! false && ! boolIdent || ! true"
Just (Or (And (Not (ValBool False)) (Not (Var "boolIdent"))) (Not (ValBool True)),"")
 ! false &&  ! boolIdent ||  ! true

LangParser> test t22
"4 && ![] || false"
Just (Or (And (ValInt 4) (Not Nil)) (ValBool False),"")
4 &&  ! [] || false

LangParser> test t23
" 4 : []"
Just (Cons (ValInt 4) Nil,"")
4 : []

LangParser> test t24
"true : false : x : []"
Just (Cons (ValBool True) (Cons (ValBool False) (Cons (Var "x") Nil)),"")
true : false : x : []

LangParser> test t25
"4 + 2 : 5 / 1 : ! false && true || z : []"
Just (Cons (Plus (ValInt 4) (ValInt 2)) (Cons (Div (ValInt 5) (ValInt 1)) (Cons (Or (And (Not (ValBool False)) (ValBool True)) (Var "z")) Nil)),"")
4 + 2 : 5 / 1 :  ! false && true || z : []

LangParser> test t26
"f 5"
Just (App (Var "f") (ValInt 5),"")
f 5

LangParser> test t27
"f g h 3"
Just (App (App (App (Var "f") (Var "g")) (Var "h")) (ValInt 3),"")
f g h 3

LangParser> test t28
" f g 8 * 2"
Just (App (App (Var "f") (Var "g")) (Mult (ValInt 8) (ValInt 2)),"")
f g 8 * 2

LangParser> test t29
"f 3 : []"
Just (App (Var "f") (Cons (ValInt 3) Nil),"")
f 3 : []

LangParser> test t30
"if true then 2 else 6"
Just (If (ValBool True) (ValInt 2) (ValInt 6),"")
if true then 2 else 6

LangParser> test t31
"if true && false then 2 + 5 else 9 : []"
Just (If (And (ValBool True) (ValBool False)) (Plus (ValInt 2) (ValInt 5)) (Cons (ValInt 9) Nil),"")
if true && false then 2 + 5 else 9 : []

LangParser> test t32
"if true then 3 else if false then 9 else 1"
Just (If (ValBool True) (ValInt 3) (If (ValBool False) (ValInt 9) (ValInt 1)),"")
if true then 3 else if false then 9 else 1

LangParser> test t33
"let x = 5 in x"
Just (Let "x" (ValInt 5) (Var "x"),"")
let x = 5 in x

LangParser> test t34
"let x = 5 * 7 in x : []"
Just (Let "x" (Mult (ValInt 5) (ValInt 7)) (Cons (Var "x") Nil),"")
let x = 5 * 7 in x : []

LangParser> test t35
"let x = true : [] in let y = 5 in y : x"
Just (Let "x" (Cons (ValBool True) Nil) (Let "y" (ValInt 5) (Cons (Var "y") (Var "x"))),"")
let x = true : [] in let y = 5 in y : x

LangParser> test t36
"let y = if 3 then 2 else true in let y = if true then 5 else [] in let x = 4 in x"
Just (Let "y" (If (ValInt 3) (ValInt 2) (ValBool True)) (Let "y" (If (ValBool True) (ValInt 5) Nil) (Let "x" (ValInt 4) (Var "x"))),"")
let y = if 3 then 2 else true in let y = if true then 5 else [] in let x = 4 in x

LangParser> test t37
"\\x -> 5"
Just (Lam "x" (ValInt 5),"")
\ x -> 5

LangParser> test t38
"\\x -> \\y -> x"
Just (Lam "x" (Lam "y" (Var "x")),"")
\ x -> \ y -> x

LangParser> test t39
"\\x -> let y = x in x && y"
Just (Lam "x" (Let "y" (Var "x") (And (Var "x") (Var "y"))),"")
\ x -> let y = x in x && y

LangParser> test t40
"\\x -> if x then true else y : []"
Just (Lam "x" (If (Var "x") (ValBool True) (Cons (Var "y") Nil)),"")
\ x -> if x then true else y : []

LangParser> test t41
"\\x -> x y"
Just (Lam "x" (App (Var "x") (Var "y")),"")
\ x -> x y

LangParser> test t42
"\\x -> x : y"
Just (Lam "x" (Cons (Var "x") (Var "y")),"")
\ x -> x : y

LangParser> test t43
"if \\x -> x then \\y -> y else \\z -> z"
Just (If (Lam "x" (Var "x")) (Lam "y" (Var "y")) (Lam "z" (Var "z")),"")
if \ x -> x then \ y -> y else \ z -> z

LangParser> test t44
"let f = \\x -> x y in \\z -> f z x"
Just (Let "f" (Lam "x" (App (Var "x") (Var "y"))) (Lam "z" (App (App (Var "f") (Var "z")) (Var "x"))),"")
let f = \ x -> x y in \ z -> f z x

LangParser> test t45
"\\x -> x + 4 : []"
Just (Lam "x" (Cons (Plus (Var "x") (ValInt 4)) Nil),"")
\ x -> x + 4 : []

LangParser> test t46
"4 * (2 + 9)"
Just (Mult (ValInt 4) (Plus (ValInt 2) (ValInt 9)),"")
4 * (2 + 9)

LangParser> test t47
"f (\\x -> x y) ( (4 : 3) x ) "
Just (App (App (Var "f") (Lam "x" (App (Var "x") (Var "y")))) (App (Cons (ValInt 4) (ValInt 3)) (Var "x")),"")
f (\ x -> x y) (4 : 3 x)

LangParser> test t48
"f (g x) (h 4)"
Just (App (App (Var "f") (App (Var "g") (Var "x"))) (App (Var "h") (ValInt 4)),"")
f (g x) (h 4)

LangParser> test t49
"3 + if true then 5 else 9"
Just (Plus (ValInt 3) (If (ValBool True) (ValInt 5) (ValInt 9)),"")
3 + (if true then 5 else 9)

LangParser> test t50
"if true then 5 else 9 + 3"
Just (If (ValBool True) (ValInt 5) (Plus (ValInt 9) (ValInt 3)),"")
if true then 5 else 9 + 3

LangParser> test t51
"(if true then 5 else 9) + 3"
Just (Plus (If (ValBool True) (ValInt 5) (ValInt 9)) (ValInt 3),"")
(if true then 5 else 9) + 3

LangParser> test t52
"2 : \\x -> x : 1"
Just (Cons (ValInt 2) (Lam "x" (Cons (Var "x") (ValInt 1))),"")
2 : (\ x -> x : 1)

LangParser> test t53
"8 - let x = 2 in 1 - 3"
Just (Minus (ValInt 8) (Let "x" (ValInt 2) (Minus (ValInt 1) (ValInt 3))),"")
8 - (let x = 2 in 1 - 3)

LangParser> test t54
"a * (b || c)"
Just (Mult (Var "a") (Or (Var "b") (Var "c")),"")
a * (b || c)

LangParser> test t55
"y - ((true (-3)) - false * false)"
Just (Minus (Var "y") (Minus (App (ValBool True) (ValInt (-3))) (Mult (ValBool False) (ValBool False))),"")
y - ((true (-3)) - false * false)

LangParser> test t56
"z :  !  ! ((-18) * [] +  ! 2)"
Just (Cons (Var "z") (Not (Not (Plus (Mult (ValInt (-18)) Nil) (Not (ValInt 2))))),"")
z :  !  ! ((-18) * [] +  ! 2)

LangParser> test t57
"! ! (-18)"
Just (Not (Not (ValInt (-18))),"")
 !  ! (-18)

-}


{-

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
--           deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq

-}


-- note that the parser must respect all the precedence and associativity rules expressed in the prettyShow function.
-- that means
-- ! binds more tightly than
-- * / which binds more tightly than
-- + - which binds more tightly than
-- && which binds more tightly than
-- || which binds more tightly than
-- : which binds more tightly than
-- {the application} which binds weakest of all

-- + - * / && || {the application} are left associative
-- : is right associative

-- we are mostly following the questionable c precedence rules

-- ungraded bonus: add additional pretty syntax for lists: [1,2,3,4]

{- CFG                       

Parser -> Apps
Apps -> Apps Cons | Cons               -- L associative
Cons -> Or Cons | Or                   -- R associative
Or -> Or "||" And                      -- L associative
Or -> And
And -> And "&&" PlusMinus              -- L associative
And -> PlusMinus
PlusMinus -> PlusMinus "+" MultDiv     -- L associative
PlusMinus -> PlusMinus "-" MultDiv     -- L associative
PlusMinus -> MultDiv
MultDiv -> MultDiv "*" Not             -- L associative
MultDiv -> MultDiv "/" Not             -- L associative
Not -> "!" Not
Not -> Atoms
Atoms -> Bools | Ints | Nil | Var | Parens | Lambda | Let | If
Bools -> "true" | "false"
Ints -> <integer>
Nil -> "[]"
Var -> <variable>
Parens -> "(" Parser ")"
Lambda -> "\" Var "->" Parser                  -- to account for escape symbol, this should be "\\"
Let -> "let" Var "=" Parser
If  -> "if" Parser "then" Parser "else" Parser

-}
