module ExecTest2 where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

import Exec
import Eval
import Check

tests = testGroup "ExecTest2: To be merged..."
  [
    testCase "List Operations: " $
      do
        assertEqual "[12,true,2.2]" (Ret (Ls ([I 12, B True, F 2.2])) [] []) (exec "[12,true,2.2]")
        assertEqual "[1,2]++[3,4]" (Ret (Ls ([I 1, I 2, I 3, I 4])) [] []) (exec "[1,2]++[3,4]")
        assertEqual
          "[1,2]++[3,true,3.2]"
          (Ret (Ls ([I 1, I 2, I 3, B True, F 3.2])) [] [])
          (exec "[1,2]++[3,true,3.2]")
        assertEqual
          "[1,2]++2"
          (RuntimeError "TypeMismatch: second argument (2) must be a list" [] [])
          (exec "[1,2]++2")
        assertEqual
          "[1,2,4,x]"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "x is not in scope"])
          (exec "[1,2,4,x]")
        assertEqual
          "[1,2,,,,,]"
          (ParseError)
          (exec "[1,2,,,,,]")
        assertEqual "[]" (Ret (Ls []) [] []) (exec "[]"),

    testCase "Arithmetic: Addition " $
      do
        assertEqual "1+5" (Ret (I 6) [] []) (exec "1+5")
        assertEqual "1.3+3.2" (Ret (F 4.5) [] []) (exec "1.3+3.2")
        assertEqual
          "1.0+5"
          (RuntimeError "TypeMismatch: Cannot add float 1.0 and integer 5" [] [])
          (exec "1.0+5")
        assertEqual
          "x+5"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "x is not in scope"])
          (exec "x+5")
        assertEqual
          "1.0+'c'"
          (RuntimeError "'c' is not a number" [] [])
          (exec "1.0+'c'")
        assertEqual "++3" (ParseError) (exec "++3"),

    testCase "Arithmetic: Subtraction " $
      do
        assertEqual "16-5" (Ret (I 11) [] []) (exec "16-5")
        assertEqual "2.2-1.0" (Ret (F 1.2) [] []) (exec "2.2-1.0")
        assertEqual
          "2.2-1"
          (RuntimeError "TypeMismatch: Cannot subtract float 2.2 and integer 1" [] [])
          (exec "2.2-1")
        assertEqual
          "2.2-true"
          (RuntimeError "true is not a number" [] [])
          (exec "2.2-true")
        assertEqual
          "x-45"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "x is not in scope"])
          (exec "x-45")
        assertEqual "-2" (Ret (I (-2)) [] []) (exec "-2")
        assertEqual "-45.92" (Ret (F (-45.92)) [] []) (exec "-45.92")
        assertEqual "2.2--" (ParseError) (exec "2.2--"),

    testCase "Arithmetic: Multiplication " $
      do
        assertEqual "3*4" (Ret (I 12) [] []) (exec "3*4")
        assertEqual "3.2*4.5" (Ret (F 14.400001) [] []) (exec "3.2*4.5")
        assertEqual
          "3.2*3"
          (RuntimeError "TypeMismatch: Cannot multiply float 3.2 and integer 3" [] [])
          (exec "3.2*3")
        assertEqual
          "3.2*x"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "x is not in scope"])
          (exec "3.2*x")
        assertEqual
          "3.2*true"
          (RuntimeError "true is not a number" [] [])
          (exec "3.2*true")
        assertEqual "8***2" (ParseError) (exec "8***2")
        assertEqual "**2" (ParseError) (exec "**2")
        assertEqual "2*" (ParseError) (exec "2*"),

    testCase "Arithmetic: Division " $
      do
        assertEqual "30//4" (Ret (I 7) [] []) (exec "30//4")
        assertEqual "4.5/3.2" (Ret (F 1.40625) [] []) (exec "4.5/3.2")
        assertEqual
          "4.5/3"
          (RuntimeError "TypeMismatch: Can only use / with Float types" [] [])
          (exec "4.5/3")
        assertEqual
          "4.5//3"
          (RuntimeError "TypeMismatch: Can only use // with Integer types" [] [])
          (exec "4.5//3")
        assertEqual
          "10/4"
          (RuntimeError "TypeMismatch: Can only use / with Float types" [] [])
          (exec "10/4")
        assertEqual
          "4.5555//3.293"
          (RuntimeError "TypeMismatch: Can only use // with Integer types" [] [])
          (exec "4.5555//3.293")
        assertEqual
          "10/y"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "y is not in scope"])
          (exec "10/y")
        assertEqual
          "14.2//z"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "z is not in scope"])
          (exec "14.2//z")
        assertEqual
          "3/true"
          (RuntimeError "true is not a number" [] [])
          (exec "3/true")
        assertEqual
          "3//'q'"
          (RuntimeError "'q' is not a number" [] [])
          (exec "3//'q'")
        assertEqual "//3.4" (ParseError) (exec "//3.4")
        assertEqual "3.4//" (ParseError) (exec "3.4//")
        assertEqual "3/"    (ParseError) (exec "3/")
        assertEqual "3//"   (ParseError) (exec "3//"),

    testCase "Arithmetic: Exponentiation " $
      do
         assertEqual "3.2^4.5" (Ret (F 187.575) [] []) (exec "3.2^4.5")
         assertEqual "3**4"    (Ret (I 81) [] []) (exec "3**4")
         assertEqual
           "3.2^4"
           (RuntimeError "TypeMismatch: Can only use ^ with Float types" [] [])
           (exec "3.2^4")
         assertEqual
           "4**5.2"
           (RuntimeError "TypeMismatch: Can only use ** with Integer types" [] [])
           (exec "4**5.2")
         assertEqual
           "true^3"
           (RuntimeError "true is not a number" [] [])
           (exec "true^3")
         assertEqual
           "4** 'c'"
           (RuntimeError "'c' is not a number" [] [])
           (exec "4**'c'")
         assertEqual
           "x^4"
           (RuntimeError "Variable not found!" [] [UndefinedVarUse "x is not in scope"])
           (exec "x^4")
         assertEqual
           "3**y"
           (RuntimeError "Variable not found!" [] [UndefinedVarUse "y is not in scope"])
           (exec "3**y")
         assertEqual "^4.5" (ParseError) (exec "^4.5")
         assertEqual "4.5^" (ParseError) (exec "4.5^")
         assertEqual "**3"  (ParseError) (exec "**3")
         assertEqual "3**"  (ParseError) (exec "3**"),

    testCase "Arithmetic: Modulus " $
      do
        assertEqual "5%3" (Ret (I 2) [] []) (exec "5%3")
        assertEqual "5%0" (RuntimeError "Error: Mod-by-Zero is undefined" [] []) (exec "5%0")
        assertEqual "true%3" (RuntimeError "true is not a number" [] []) (exec "true%3")
        assertEqual
          "x%3"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "x is not in scope"])
          (exec "x%3")
        assertEqual
          "4.5%3"
          (RuntimeError "TypeMismatch: Can only use % with Integer types" [] [])
          (exec "4.5%3"),

    testCase "Heterogeneous Arithmetic " $
      do
        assertEqual "5+3-10*2" (Ret (I (-12)) [] []) (exec "5+3-10*2")
        assertEqual "5-10//3" (Ret (I 2) [] []) (exec "5-10//3")
        assertEqual "10**2+5//2" (Ret (I 102) [] []) (exec "10**2+5//2")
        assertEqual "4.2^7.9/80.2" (Ret (F 1045.9115) [] []) (exec "4.2^7.9/80.2")
        assertEqual "5+3--10*2" (Ret (I 28) [] []) (exec "5+3--10*2")
        assertEqual "5%2+1*10"  (Ret (I 11) [] []) (exec "5%2+1*10")
        assertEqual
          "5+1-true*2"
          (RuntimeError "true is not a number" [] [])
          (exec "5+1-true*2")
        assertEqual
          "5-x//32"
          (RuntimeError "Variable not found!" [] [UndefinedVarUse "x is not in scope"])
          (exec "5-x//32")
        assertEqual
          "2**3^4"
          (RuntimeError "TypeMismatch: Can only use ^ with Float types" [] [])
          (exec "2**3^4")
        assertEqual "(4)///5+1" (ParseError) (exec "(4)///5+1")
        assertEqual "5+-" (ParseError) (exec "5+-")
        assertEqual "%//4" (ParseError) (exec "%//4")
  ]
