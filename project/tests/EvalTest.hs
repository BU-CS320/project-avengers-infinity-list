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
  testCase "Basic Ints/Bools/Floats/Chars/Strings/Vars: " $
    do 
      assertEqual "Bool True "   (Ok $ B True) (run (ValBool True))
      assertEqual "Bool False "  (Ok $ B True) (run (ValBool True))
      assertEqual "Ints "        (Ok $ I 2)    (run (ValInt 2))
      assertEqual "Ints "        (Ok $ I (-10)) (run (ValInt (-10)))
      assertEqual "Ints "        (Ok $ I 0)    (run (ValInt (0)))
      assertEqual "Floats "      (Ok $ F 1.1)  (run (ValFloat 1.1))
      assertEqual "Floats "      (Ok $ F (-3.14159)) (run (ValFloat (-3.14159))),
  testCase "Basic Arithmetic: " $
    do
      assertEqual "2 + 4 =? "    (Ok $ I 6)    (run (Plus (ValInt 2) (ValInt 4)))
      assertEqual "2 - 4 =? "    (Ok $ I (-2)) (run (Minus (ValInt 2) (ValInt 4)))
      assertEqual "3 * 2 =? "    (Ok $ I 6)    (run (Mult (ValInt 3) (ValInt 2)))
      
  ]

