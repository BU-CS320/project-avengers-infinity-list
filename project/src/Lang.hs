module Lang where

import Data.Map (Map)
import Data.Set (Set)
import Data.List (isSubsequenceOf)
import qualified Data.Map as Map
import qualified Data.Set as Set --used for lambda stuff

import HelpShow

import EnvUnsafe


-- some helper function, you may find helpful
valOf :: String -> EnvUnsafe Env Val
valOf var = do env <- getEnv
               case (Map.lookup var env) of
                  Just i  -> return i
                  Nothing -> err "Variable not found!"

-- add a val into the environment
withVal :: String -> Val -> EnvUnsafe Env a -> EnvUnsafe Env a
withVal var v comp = undefined

-- helper functions that take care of type issues (use a "Error" when things have the wron type
evalNum :: Ast -> EnvUnsafe Env (Either Float Integer)
evalNum a = do a' <- eval a
               case a' of
                 F f -> return (Left f)
                 I i -> return (Right i)
                 _   -> err "Not a number"

evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a = do a' <- eval a
               case a' of
                 I i -> return i
                 _ -> err "Not an int"

evalFloat :: Ast -> EnvUnsafe Env Float
evalFloat a = do a' <- eval a
                 case a' of
                   F f -> return f
                   _ -> err "Not a float"

evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a = do a' <- eval a
                case a' of
                  B b -> return b
                  _ -> err "Not a bool"

evalList :: Ast -> EnvUnsafe Env [Val]
evalList a = do a' <- eval a
                case a' of
                  Ls x -> return x
                  _ -> err "Not a list"

evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
evalFun a = do a' <- eval a
               case a' of
                 Fun a' -> return a'
                 _ -> err "Not a function"

--COPIED FROM HW8--not sure if this even works, but it's worth a shot (for the let expression)
local :: (r -> r) -> EnvUnsafe r a -> EnvUnsafe r a
local changeEnv comp  = EnvUnsafe (\e -> runEnvUnsafe comp (changeEnv e) )


-- ...
-- ungraded bonus challenge: use a helper type class to do this functionality


--some examples:

e1 = showPretty (Minus (ValInt 100) (Minus (ValInt 2) (ValInt 5))) $  0
e2 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (ValInt 5) ) $  0

e3 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e4 = showPretty (Div (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e5 = showPretty (((Var "fun") `App` (ValInt 2)) `App` (ValInt 5)) $ 0

e6 = showPretty (Not $ Not $ ((Var "fun") `App` (ValInt 2)) `App` (Not $ ValInt 5)) $ 0

example = let x = Var "x"
          in App (Lam "x" ( x `Plus` x))  (ValInt 7)
example' = run example

example2 = let x = Var "x"; y = Var "y"
           in ((Lam "x" (Lam "y" ( x `Plus` y))) `App` (ValInt 7)) `App` (ValInt 4)
example2' = run example2

example3 = let x = Var "x"; y = Var "y"
           in Lam "x" (Lam "y" ( x `Plus` y))
example3' = run example3

example4 = let x = Var "x"; y = Var "y"
           in (Cons $ Lam "x" (Lam "y" ( x `Plus` y))) $  (Cons ( Lam "x" (Lam "y" ( x `Minus` y))) $ Nil)
example4' = run example4

-- Here is the abstract syntax tree for our language

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer
         | ValFloat Float
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast
         | IntExp Ast Ast
         | NegExp Ast

         | Nil
         | Cons Ast Ast
         | ListIndex Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
         deriving (Eq,Show) -- helpful to use this during testing
         --deriving Eq

--instance Show Ast where
--  show ast = showPretty ast 0


-- the goal of the program is to return a value
data Val = I Integer | B Bool | F Float | C Char
         | Ls [Val] | S [Char]
         | Fun (Val -> Unsafe Val) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (F f) = show f
  show (B b) = show b
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function


stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> Ok $ Ls ls
                                   _         -> Error "can only call tail on a non empty list"),
   ("head", Fun $ \ v -> case v of Ls (head:_) -> Ok $ head
                                   _           -> Error "can only call head on a non empty list"),
   ("len", Fun $ \ v -> case v of Ls ls -> Ok $ (I (fromIntegral (length ls)))
                                  _     -> Error "")]

-- helper function that runs with a standard library of functions: head, tail ...
run :: Ast -> Unsafe Val
run a = runEnvUnsafe (eval a) stdLib

type Env = Map String Val

eval :: Ast -> EnvUnsafe Env Val
eval (ValBool bool) = return (B bool)
eval (ValInt int) = return (I int)
eval (ValFloat flo) = return (F flo)
eval (Var str) = valOf str
eval (And x y) =
  do x' <- evalBool x
     y' <- evalBool y
     return (B (x' && y'))
eval (Or x y) =
  do x' <- evalBool x
     y' <- evalBool y
     return (B (x' || y'))
eval (NegExp x) =
  do x' <- evalNum x
     case x' of
       Left f -> return (F (0 - f))
       Right i -> return (I (0 - i))
eval (Not x) =
  do x' <- evalBool x
     return (B (not x'))
eval (IntExp b e) =
  do b' <- evalInt b
     e' <- evalInt e
     return (I (b' ^ e'))
eval (Plus x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2)   -> return (F (f1 + f2))
       (Right i1, Right i2) -> return (I (i1 + i2))
       (Right _, Left _)    -> err "TypeMismatch: Cannot add integer and float"
       (Left _, Right _)    -> err "TypeMismatch: Cannot add float and integer"
eval (Minus x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2)   -> return (F (f1 - f2))
       (Right i1, Right i2) -> return (I (i1 - i2))
       (Right _, Left _)    -> err "TypeMismatch: Cannot subtract integer and float"
       (Left _, Right _)    -> err "TypeMismatch: Cannot subtract float and integer"
eval (Mult x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2)   -> return (F (f1 * f2))
       (Right i1, Right i2) -> return (I (i1 * i2))
       (Right _, Left _)    -> err "TypeMismatch: Cannot multiply integer and float"
       (Left _, Right _)    -> err "TypeMismatch: Cannot multiply float and integer"
eval (Div x y) = --not changing this one because div by 0
  do x' <- eval x
     y' <- eval y
     case (x') of
       I xInt -> case (y') of
         I 0 -> err "Divide by 0 error"
         I yInt -> return (I (xInt + yInt))
         _ -> err "Invalid types"
       _ -> err "Invalid types"
eval (Nil) = return (Ls [])
eval (Cons x y) =
  do x' <- eval x
     y' <- eval y
     case (y') of
       Ls list -> return (Ls (x':list))
       _ -> err "Second term must be a list"
eval (ListIndex lst idx) =
  do lst' <- evalList lst
     idx' <- evalInt idx
     case (validListIndex lst' idx') of
       Left errorMsg -> err errorMsg
       Right val -> return val
eval (If condition ifTrue ifFalse) =
  do condition' <- eval condition
     case (condition') of
       B True -> eval ifTrue
       B False -> eval ifFalse
       _ -> err "Condition must evaluate to a boolean"
eval (Let var val bod) =
  do val' <- eval val
     local (Map.insert var val') (eval bod)
eval (App x y) =
  do x' <- evalFun x
     y' <- eval y
     case (x' y') of
       Ok val -> return val
       _ -> err "Invalid function argument"
eval (Lam x bod) =
  do env <- getEnv
     return (Fun (\v -> runEnvUnsafe (eval bod) (Map.insert x v env)))

testlam1 = Lam "x" (Plus (Var "x") (ValInt 4))
testlam2 = App testlam1 (ValInt 3)


validListIndex :: [Val] -> Integer -> Either String Val
validListIndex lst idx
  | (fromIntegral idx) >= (length lst) =
    Left $ "Index too large. Index given: " ++ (show idx) ++ " but maximum is: " ++ (show ((length lst) - 1))
  | otherwise          = Right (lst !! (fromIntegral idx))


-- This is helpful for testing and debugging
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValFloat f) = "(" ++ show f ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (NegExp x) = "(-" ++ (showFullyParen x) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (IntExp b e) = "(" ++ (showFullyParen b) ++ " ** " ++ (showFullyParen e) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"
showFullyParen (ListIndex lst idx) = "(" ++ (showFullyParen lst) ++ " !! " ++ (showFullyParen idx) ++ ")"


-- provide a nice show with minimal parentheses, for testing an documentation



--the bigger the number the more tight the biding
showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValFloat f) _ =  if f < 0
                             then  "(" ++ show f ++ ")"
                             else show f
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s

showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (App l r) i = parenthesize 2 i $ (showPretty l 2) ++ " " ++ (showPretty r 3)
showPretty (Cons l r) i = parenthesize 4 i $ (showPretty l 5) ++ " : " ++ (showPretty r 4)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)
showPretty (Minus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " - " ++ (showPretty r 11)
showPretty (Plus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " + " ++ (showPretty r 11)
showPretty (Mult l r) i = parenthesize 12 i $ (showPretty l 12) ++ " * " ++ (showPretty r 13)
showPretty (Div l r) i = parenthesize 12 i $ (showPretty l 12) ++ " / " ++ (showPretty r 13)
showPretty (IntExp b e) i = parenthesize 13 i $ (showPretty b 13) ++ " ** " ++ (showPretty e 14)
showPretty (ListIndex lst idx) i = parenthesize 14 i $ (showPretty lst 14) ++ " !! " ++ (showPretty idx 14)
showPretty (NegExp x) i = parenthesize 14 i $ " - " ++ (showPretty x 14)
showPretty (Not l ) i = parenthesize 14 i $  " ! " ++ (showPretty l 14)


