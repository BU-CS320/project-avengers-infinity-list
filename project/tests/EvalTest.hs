module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

--import Ast
--import Eval
import Lang
import EnvUnsafe

-- provide tests that show your run/eval works
{-
eqInt :: Val -> Val -> Bool
eqInt (I i) (I i') = i == i'
eqInt _ _ = False

eqFloat :: Val -> Val -> Bool
eqFloat (F f) (F f') = f == f'
eqFloat _ _ = False

eqBool :: Val -> Val -> Bool
eqBool (B b) (B b') = b == b'
eqBool _ _ = False

eqChar :: Val -> Val -> Bool
eqChar (C c) (C c') = c == c'
eqChar _ _ = False

eqString :: Val -> Val -> Bool
eqString (S s) (S s') = s == s'
eqString _ _ = False

eqList :: Val -> Val -> Bool
eqList (Ls (x:xs)) (Ls (y:ys)) = (x == y) && ((Ls xs) == (Ls ys))
eqList _ _ = False

eqFun :: Val -> Val -> Bool
eqFun (Fun f) (Fun g) = f == g
eqFun _ _ = False
-}

tests = testGroup "EvalTest"
  [
  testCase "Basic Ints/Bools/Floats/Chars/Strings: " $
    do
      assertEqual "Bool True "   (Ok $ B True) (run (ValBool True))
      assertEqual "Bool False "  (Ok $ B True) (run (ValBool True))
      assertEqual "Ints "        (Ok $ I 2)    (run (ValInt 2))
      assertEqual "Ints "        (Ok $ I (-10)) (run (ValInt (-10)))
      assertEqual "Ints "        (Ok $ I 0)    (run (ValInt (0)))
      assertEqual "Floats "      (Ok $ F 1.1)  (run (ValFloat 1.1))
      assertEqual "Floats "      (Ok $ F (-3.14159)) (run (ValFloat (-3.14159)))
      assertEqual "Chars "       (Ok $ C 'a')  (run (ValChar 'a'))
      assertEqual "Chars "       (Ok $ C 'z')  (run (ValChar 'z'))
      assertEqual "Strings "     (Ok $ S "asdf")  (run (ValString "asdf"))
      assertEqual "Strings "     (Ok $ S "fe.d")  (run (ValString "fe.d")),
  testCase "Basic Arithmetic: " $
    do
      assertEqual "2 + 4 =? "    (Ok $ I 6)    (run (Plus (ValInt 2) (ValInt 4)))
      assertEqual "2 - 4 =? "    (Ok $ I (-2)) (run (Minus (ValInt 2) (ValInt 4)))
      assertEqual "3 * 2 =? "    (Ok $ I 6)    (run (Mult (ValInt 3) (ValInt 2)))
      assertEqual "3 / 2 =? "    (Ok $ I 1)    (run (Div (ValInt 3) (ValInt 2)))
      assertEqual "2 ** 4 =? "   (Ok $ I 16)   (run (IntOrFloatExp (ValInt 2) (ValInt 4))),
  testCase "Comparison Operators: " $
    do
      assertEqual "3 == 3 =? "   (Ok $ B True)  (run (Equals (ValInt 3) (ValInt 3)))
      assertEqual "3 != 4 =? "   (Ok $ B True)  (run (NotEquals (ValInt 3) (ValInt 4)))
      assertEqual "4 < 5 =? "    (Ok $ B True)  (run (LessThan (ValInt 4) (ValInt 5)))
      assertEqual "5 < 4 =? "    (Ok $ B False) (run (LessThan (ValInt 5) (ValInt 4)))
      assertEqual "4 > 5 =? "    (Ok $ B False) (run (GreaterThan (ValInt 4) (ValInt 5)))
      assertEqual "5 > 4 =? "    (Ok $ B True)  (run (GreaterThan (ValInt 5) (ValInt 4)))
      assertEqual "4 <= 5 =? "    (Ok $ B True)  (run (LessThanOrEquals (ValInt 4) (ValInt 5)))
      assertEqual "5 <= 4 =? "    (Ok $ B False) (run (LessThanOrEquals (ValInt 5) (ValInt 4)))
      assertEqual "4 >= 5 =? "    (Ok $ B False) (run (GreaterThanOrEquals (ValInt 4) (ValInt 5)))
      assertEqual "5 >= 4 =? "    (Ok $ B True)  (run (GreaterThanOrEquals (ValInt 5) (ValInt 4))),
  testCase "Boolean Operators: " $
    do
      assertEqual "True and True" (Ok $ B True) (run (And (ValBool True) (ValBool True)))
      assertEqual "True and False" (Ok $ B False) (run (And (ValBool True) (ValBool False)))
      assertEqual "False and True" (Ok $ B False) (run (And (ValBool False) (ValBool True)))
      assertEqual "False and False" (Ok $ B False) (run (And (ValBool False) (ValBool False)))
      assertEqual "True or True" (Ok $ B True) (run (Or (ValBool True) (ValBool True)))
      assertEqual "True or False" (Ok $ B True) (run (Or (ValBool True) (ValBool False)))
      assertEqual "False or True" (Ok $ B True) (run (Or (ValBool False) (ValBool True)))
      assertEqual "False or False" (Ok $ B False) (run (Or (ValBool False) (ValBool False))),
  testCase "If Condition: " $
    do
      assertEqual "if 3 then 4 else 2 =? " (Ok $ I 4) (run (If (ValBool True) (ValInt 4) (ValInt 2)))
      assertEqual "if 0 then 1 else 4 =? " (Ok $ I 4) (run (If (ValBool False) (ValInt 1) (ValInt 4))),
  testCase "Separator: " $
    do
      assertEqual "3;4 =? "    (Ok $ I 4) (run (Separator (ValInt 3) (ValInt 4)))
      assertEqual "2;10 =? "   (Ok $ I 10) (run (Separator (ValInt 2) (ValInt 10))),
  testCase "Let Statements: " $
    do
      assertEqual "let x = 4 in x * 2 =? " (Ok $ I 8) (run (Let ("x") (ValInt 4) (Mult (ValString "x") (ValInt 2))))
      assertEqual "let x = 4 in x - 1 =? " (Ok $ I 3) (run (Let ("x") (ValInt 4) (Minus (ValString "x") (ValInt 1)))),
  testCase "App/Lam Statements: " $
    do
      assertEqual "((lam)x -> x) 3" (Ok $ S "x") (run (App (Lam ("x") (ValString "x")) (ValInt 3)))
  ]

