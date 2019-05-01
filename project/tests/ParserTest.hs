module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck 

import Lang (showFullyParen, showPretty, Ast(..))
import ParserMonad (parse)
--import Ast
--import Parser
import LangParser (parser)
import HelpShow
-- provide tests that show your parser works

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i    <- arbitrary -- will choose a random Integer
                                 b    <- arbitrary
                                 node <- elements [Nil, ValInt i, ValBool b]  -- so put all the non-recursive Ast expressions here
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)  -- get ast half as big
                                     r <- arbitrarySizedAst (m `div` 2)  -- ditto
                                     x <- elements ["x", "y", "z"]   -- will choose random element from the list
                                     ifAst <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l,
                                                       Plus l r, Minus l r, Mult l r, Div l r,
                                                       Equals l r, NotEquals l r, LessThan l r, GreaterThan l r, LessThanOrEquals l r, GreaterThanOrEquals l r,
                                                       IntOrFloatExp l r,
                                                       Cons l r,
                                                       ListIndex l r, ifAst,
                                                       Let x l r, App l r, Lam x l
                                                      ]
                                     return node

-- break in thirds for mix-fix operators which have three separate sub-asts

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
