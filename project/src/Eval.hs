module Eval where

import Data.Map (Map)
import Data.Set (Set)
import Data.List (isSubsequenceOf)
import Data.Char
import Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set

import HelpShow
import Ast
import EnvUnsafeLog

-- the goal of the program is to return a value
data Val = I Integer | B Bool | F Float | C Char
         | Ls [Val] | S [Char]
         | Fun (Val -> (Unsafe Val, [String])) -- since this is a functional language, one thing that can be returned is a function
         | Err String

instance Show Val where
  show (I i) = show i
  show (F f) = show f
  show (B b) = show b
  show (C c) = show c
  show (S s) = show s
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function
  show (Err msg) = "Error: " ++ msg

instance Eq Val where
  (I x) == (I y) = x == y
  (F x) == (F y) = x == y
  (B x) == (B y) = x == y
  (C x) == (C y) = x == y
  (S x) == (S y) = x == y
  (Ls []) == (Ls []) = True
  (Ls (x:xs)) == (Ls (y:ys)) = (x == y) && ((Ls xs) == (Ls ys))
  _ == _ = False

type Env = Map String Val

stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> ((Ok $ Ls ls), [])
                                   _         -> (Error "can only call tail on a non empty list", [])),
   ("head", Fun $ \ v -> case v of Ls (head:_) -> ((Ok $ head), [])
                                   _           -> (Error "can only call head on a non empty list", [])),
   ("len", Fun $ \ v -> case v of Ls ls -> ((Ok $ (I (fromIntegral (length ls)))), [])
                                  _     -> (Error "can only call length on a list", [])),
   ("ord", Fun $ \ v -> case v of C ch -> ((Ok $ (I (fromIntegral (ord ch)))), [])
                                  _    -> (Error "can only call ord on a char", [])),
   ("chr", Fun $ \ v -> case v of I integer -> ((Ok $ (C (chr (fromIntegral integer)))), [])
                                  _    -> (Error "can only call chr on a char", [])),
   ("float", Fun $ \ v -> case v of I integer -> ((Ok $ (F (realToFrac (fromIntegral integer)))), [])
                                    _    -> (Error "can only call float on an int", [])),
   ("int", Fun $ \ v -> case v of F fl -> ((Ok $ (F (fromIntegral (truncate fl)))), [])
                                  _    -> (Error "can only call int on a float", [])),
   ("elem", Fun $ \ var -> ((Ok $ Fun $ \ list -> case list of
                                             Ls ls -> ((Ok $ (B (elem var ls))), [])
                                             _     -> (Error "can only call elem on a list", [])), [])),
   ("filter", Fun $ \ func -> ((Ok $ Fun $ \ list -> case list of
                                             Ls ls -> case func of
                                               Fun fn -> ((Ok $ (Ls (filterHelper fn ls))), [])
                                               _ -> (Error "first argument of filter must be a function", [])
                                             _     -> (Error "can only call filter on a list", [])), [])),
   ("map", Fun $ \ func -> ((Ok $ Fun $ \ list -> case list of
                                             Ls ls -> case func of
                                               Fun fn -> ((Ok $ Ls (mapHelper fn ls)), [])
                                               _ -> (Error "first argument of map must be a function", [])
                                             _     -> (Error "can only call map on a list", []), [])))]

local :: (r -> r) -> EnvUnsafeLog r String a -> EnvUnsafeLog r String a
local changeEnv comp  = EnvUnsafeLog (\e -> runEnvUnsafe comp (changeEnv e) )

filterHelper :: (a -> (Unsafe Val, [b])) -> [a] -> [a]
filterHelper _ [] = []
filterHelper func (head:body) = case (func head) of
  (Ok (B True), log) -> [head] ++ (filterHelper func body)
  (Ok (B False), log) -> (filterHelper func body)
  _ -> (head:body)

mapHelper :: (Val -> (Unsafe Val, [b])) -> [Val] -> [Val]
mapHelper _ [] = []
mapHelper func (head:body) = let (res, log) = func head in case res of
                                                             (Ok val) -> val:(mapHelper func body)
                                                             (Error msg) -> head:body

validListIndex :: [Val] -> Integer -> Either String Val
validListIndex lst idx
  | (fromIntegral idx) >= (length lst) =
    Left $ "Index too large. Index given: " ++ (show idx) ++ " but maximum is: " ++ (show ((length lst) - 1))
  | otherwise          = Right (lst !! (fromIntegral idx))

valOf :: String -> EnvUnsafeLog Env String Val
valOf var = do env <- getEnv
               case (Map.lookup var env) of
                  Just i  -> return i
                  Nothing -> err "Variable not found!"


-- helper functions that take care of type issues (use a "Error" when things have the wron type
evalNum :: Ast -> EnvUnsafeLog Env String (Either Float Integer)
evalNum a = do a' <- eval a
               case a' of
                 F f -> return (Left f)
                 I i -> return (Right i)
                 _   -> err "Not a number"

evalInt :: Ast -> EnvUnsafeLog Env String Integer
evalInt a = do a' <- eval a
               case a' of
                 I i -> return i
                 _ -> err "Not an int"

evalFloat :: Ast -> EnvUnsafeLog Env String Float
evalFloat a = do a' <- eval a
                 case a' of
                   F f -> return f
                   _ -> err "Not a float"

evalChar :: Ast -> EnvUnsafeLog Env String Char
evalChar a = do a' <- eval a
                case a' of
                  C c -> return c
                  _ -> err "Not an int"

evalBool :: Ast -> EnvUnsafeLog Env String Bool
evalBool a = do a' <- eval a
                case a' of
                  B b -> return b
                  _ -> err "Not a bool"

evalList :: Ast -> EnvUnsafeLog Env String [Val]
evalList a = do a' <- eval a
                case a' of
                  Ls x -> return x
                  _ -> err "Not a list"

evalFun :: Ast -> EnvUnsafeLog Env String (Val -> (Unsafe Val, [String]))
evalFun a = do a' <- eval a
               case a' of
                 Fun a' -> return a'
                 _ -> err "Not a function"

evalPrint :: Ast -> EnvUnsafeLog Env String Val
evalPrint a = do a' <- eval a
                 let str = show a'
                 printBuffer str
                 return a'

equals :: Ast -> Ast -> EnvUnsafeLog Env String Val
equals x y =
  do x' <- eval x
     y' <- eval y
     return (B (x' == y'))

notEquals :: Ast -> Ast -> EnvUnsafeLog Env String Val
notEquals x y =
  do x' <- eval x
     y' <- eval y
     return (B (not $ x' == y'))

lessThan :: Ast -> Ast -> EnvUnsafeLog Env String Val
lessThan x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' < y''))
       (F x'', F y'') -> return (B (x'' < y''))
       (B x'', B y'') -> return (B (x'' < y''))
       (C x'', C y'') -> return (B (x'' < y''))
       (S x'', S y'') -> return (B (x'' < y''))
       _              -> return (B False)

lessThanOrEquals :: Ast -> Ast -> EnvUnsafeLog Env String Val
lessThanOrEquals x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' <= y''))
       (F x'', F y'') -> return (B (x'' <= y''))
       (B x'', B y'') -> return (B (x'' <= y''))
       (C x'', C y'') -> return (B (x'' <= y''))
       (S x'', S y'') -> return (B (x'' <= y''))
       _              -> return (B False)

greaterThan :: Ast -> Ast -> EnvUnsafeLog Env String Val
greaterThan x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' > y''))
       (F x'', F y'') -> return (B (x'' > y''))
       (B x'', B y'') -> return (B (x'' > y''))
       (C x'', C y'') -> return (B (x'' > y''))
       (S x'', S y'') -> return (B (x'' > y''))
       _              -> return (B False)

greaterThanOrEquals :: Ast -> Ast -> EnvUnsafeLog Env String Val
greaterThanOrEquals x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' >= y''))
       (F x'', F y'') -> return (B (x'' >= y''))
       (B x'', B y'') -> return (B (x'' >= y''))
       (C x'', C y'') -> return (B (x'' >= y''))
       (S x'', S y'') -> return (B (x'' >= y''))
       _              -> return (B False)

eval :: Ast -> EnvUnsafeLog Env String Val
eval (ValBool bool) = return (B bool)
eval (ValInt int) = return (I int)
eval (ValChar char) = return (C char)
eval (ValString str) = return (S str)
eval (ValFloat flo) = return (F flo)
eval (Var str) = valOf str
eval (Equals x y) = equals x y
eval (NotEquals x y) = notEquals x y
eval (LessThan x y) = lessThan x y
eval (LessThanOrEquals x y) = lessThanOrEquals x y
eval (GreaterThan x y) = greaterThan x y
eval (GreaterThanOrEquals x y) = greaterThanOrEquals x y
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
eval (IntDiv x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Right i1, Right i2) -> if i2 == 0 then err "Error: Division-by-Zero" else return (I (i1 `div` i2))
       _ -> err "TypeMismatch: Can only use // with Integer types"
eval (FloatDiv x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2) -> if f2 == 0.0 then err "Error: Division-by-Zero" else return (F (f1 / f2))
       _ -> err "TypeMismatch: Can only use / with Float types"
eval (IntExp b e) =
  do b' <- evalNum b
     e' <- evalNum e
     case (b', e') of
       (Right i1, Right i2) -> return (I (i1 ^ i2))
       _ -> err "TypeMismatch: Can only use ** with Integer types"
eval (FloatExp b e) =
  do b' <- evalNum b
     e' <- evalNum e
     case (b', e') of
       (Left f1, Left f2) -> return (F (f1 ** f2))
       _ -> err "TypeMismatch: Can only use ^ with Float types"
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
eval (Separator ast1 ast2) =
  do ast1' <- eval ast1
     ast2' <- eval ast2
     return ast2'
eval (Print ast) =
  do ast' <- evalPrint ast
     return ast'
eval (Let var val bod) =
  do val' <- eval val
     local (Map.insert var val') (eval bod)
eval (App x y) =
  do x' <- evalFun x
     y' <- eval y
     case (x' y') of
       (Ok val, lst) -> return val
       _ -> err "Invalid function argument"
eval (Lam x bod) =
  do env <- getEnv
     return (Fun (\v -> runEnvUnsafe (eval bod) (Map.insert x v env)))
eval (Compose f g) =
  do f' <- evalFun f
     g' <- evalFun g
     return (Fun $ \x -> case (g' x) of
       (Ok val, _) -> f' val
       (Error msg, ls) -> (Error msg, ls))


-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Unsafe Val, [String])  -- ^ (error message or result value, all the printings)
run a = runEnvUnsafe (eval a) stdLib

