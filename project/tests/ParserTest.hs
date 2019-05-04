module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck

--import Lang (showFullyParen, showPretty, Ast(..))
import ParserMonad (parse)
import Ast
import Parser
import Eval
--import LangParser (parser)
import HelpShow
-- provide tests that show your parser works

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i    <- arbitrary -- will choose a random Integer
                                 b    <- arbitrary
                                 node <- elements [Nil, ValInt (abs i), ValBool b]  -- so put all the non-recursive Ast expressions here
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)  -- get ast half as big
                                     r <- arbitrarySizedAst (m `div` 2)  -- ditto
                                     q <- elements [Nil, Cons l Nil]
                                     x <- elements ["x", "y", "z"]   -- will choose random element from the list
                                     ifAst <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l,
                                                       Plus l r, Minus l r, Mult l r, Div l r,
                                                       Equals l r, NotEquals l r, LessThan l r, GreaterThan l r, LessThanOrEquals l r, GreaterThanOrEquals l r,
                                                       IntExp l r, FloatExp l r,


                                                       Cons l Nil,
                                                       ListIndex l r, ifAst,
                                                       Let x l r, App l r, Lam x l, NegExp l
                                                      ]
                                     return node

-- break in thirds for mix-fix operators which have three separate sub-asts

e1 = showPretty (Minus (ValInt 100) (Minus (ValInt 2) (ValInt 5))) $  0
e2 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (ValInt 5) ) $  0

e3 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e4 = showPretty (Div (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e5 = showPretty (((Var "fun") `App` (ValInt 2)) `App` (ValInt 5)) $ 0

e6 = showPretty (Not $ Not $ ((Var "fun") `App` (ValInt 2)) `App` (Not $ ValInt 5)) $ 0
e7 = showPretty (Equals (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0

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

ex0 = showPretty  (Mult (Var "a") (Or (Var "b") (Var "c"))) 0

ex1 = showPretty (Minus (Var "y") (Minus (App (ValBool True) (ValInt (-3))) (Mult (ValBool False) (ValBool False)))) 0

ex2 = showPretty (Cons (Var "z") (Not (Not (Plus (Mult (ValInt (-18)) Nil) (Not (ValInt 2)))))) 0

ex3 = "! ! (-18)"

ex4 = "(1 - false >= (if (-8) then true else true) && [] / (-7) < false * []) > (let z = (true false) == (-5) / (-6) in (false && 7) + ((-9) : (-2)))"

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


arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do x <- arbitrarySizedAst (m `div` 3)
                        y <- arbitrarySizedAst (m `div` 3)
                        z <- arbitrarySizedAst (m `div` 3)
                        return $ If x y z

tests = testGroup "parser Test"
      [
      testProperty "parse should return the same AST when fully parenthesized" $
                  ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),

      testProperty "parse should return the same AST when pretty printed" $
                  ((\ x -> Just (x , "") == (parse parser $ showPretty x 0)) :: Ast -> Bool)
      ]
