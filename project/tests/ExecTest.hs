module ExecTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

import Exec
import Eval
import Check
import EnvUnsafeLog

tests = testGroup "ExecTest"
  [
  testGroup "Basic Expressions"
    [
    testCase "Atomic Values: " $
     do
      assertEqual "3" (Ret (I 3) [] []) (exec "3")
      assertEqual "3," (ParseError) (exec "3,")
      assertEqual "3.0" (Ret (F 3.0) [] []) (exec "3.0")
      assertEqual ",3.0" (ParseError) (exec ",3.0")
      assertEqual "true" (Ret (B True) [] []) (exec "true")
      assertEqual "false" (Ret (B False) [] []) (exec "false")
      assertEqual "aaa" (RuntimeError "Variable aaa is not defined or is not in scope" [] [UndefinedVarUse "aaa is not in scope"]) (exec "aaa")
      assertEqual "\"yo dawg\"" (Ret (S "yo dawg") [] []) (exec "\"yo dawg\"")
      assertEqual "\"\"" (Ret (S "") [] []) (exec "\"\"")
      assertEqual "\"\"\"" (ParseError) (exec "\"\"\"")
      assertEqual "\'a\'" (Ret (C 'a') [] []) (exec "\'a\'")
      assertEqual "[1,2,3]" (Ret (Ls ([I 1,I 2,I 3])) [] []) (exec "[1,2,3]")
      assertEqual "[\"a\", \"b\", \"c\"]" (Ret (Ls ([S "a", S "b", S "c"])) [] []) (exec "[\"a\", \"b\", \"c\"]")
      assertEqual "[\'a\', \'b\', \'c\']" (Ret (Ls ([C 'a', C 'b', C 'c'])) [] []) (exec "[\'a\', \'b\', \'c\']")
      assertEqual "[1.5, 2.5, 3.5]" (Ret (Ls ([F 1.5,F 2.5,F 3.5])) [] []) (exec "[1.5,2.5,3.5]")
      assertEqual "[]" (Ret (Ls []) [] []) (exec "[]")
      assertEqual "[]]" (ParseError) (exec "[]]")
      assertEqual
        "[1,2]adgdgsd"
        (RuntimeError "[1,2] is not a function" [] [UndefinedVarUse "adgdgsd is not in scope"])
        (exec "[1,2]adgdgsd")
      assertEqual
        "yodawg[1,2]"
        (RuntimeError "Variable yodawg is not defined or is not in scope" [] [UndefinedVarUse "yodawg is not in scope"])
        (exec "yodawg[1,2]")
      assertEqual
        "yo dawg 3.0"
        (RuntimeError "Variable yo is not defined or is not in scope" [] [UndefinedVarUse "dawg is not in scope",UndefinedVarUse "yo is not in scope"])
        (exec "yo dawg 3.0")
      assertEqual "[sdgsdg" (ParseError) (exec "[sdgsdg")
      assertEqual "[1,2 3]" (ParseError) (exec "[1,2 3]")
    ],
  testGroup "Homogeneous Boolean Expressions"
    [
    testCase "Boolean And: " $
      do
        assertEqual "true && true" (Ret (B True) [] []) (exec "true && true")
        assertEqual "true && false" (Ret (B False) [] []) (exec "true && false")
        assertEqual "false && false" (Ret (B False) [] []) (exec "false && false")
        assertEqual "false && true" (Ret (B False) [] []) (exec "false && true")
        assertEqual "false && 3" (RuntimeError "3 is not a bool" [] []) (exec "false && 3")
        assertEqual "3 && true" (RuntimeError "3 is not a bool" [] []) (exec "3 && true")
        assertEqual "&& true" (ParseError) (exec "&& true")
        assertEqual "true &&" (ParseError) (exec "true &&"),
    testCase "Boolean Or: " $
      do
        assertEqual "true || true" (Ret (B True) [] []) (exec "true || true")
        assertEqual "true || false" (Ret (B True) [] []) (exec "true || false")
        assertEqual "false || false" (Ret (B False) [] []) (exec "false || false")
        assertEqual "false || true" (Ret (B True) [] []) (exec "false || true")
        assertEqual "false || 3" (RuntimeError "3 is not a bool" [] []) (exec "false || 3")
        assertEqual "3 || true" (RuntimeError "3 is not a bool" [] []) (exec "3 || true")
        assertEqual "|| true" (ParseError) (exec "|| true")
        assertEqual "true ||" (ParseError) (exec "true ||"),
    testCase "Boolean Not: " $
      do
        assertEqual "! true" (Ret (B False) [] []) (exec "! true")
        assertEqual "! false" (Ret (B True) [] []) (exec "! false")
        assertEqual "! ! true" (Ret (B True) [] []) (exec "! ! true")
        assertEqual "! 3" (RuntimeError "3 is not a bool" [] []) (exec "! 3")
    ],
  testGroup "Heterogeneous Boolean Expressions"
    [
    testCase "&& + ||: " $
      do
        assertEqual "true && false || true" (Ret (B True) [] []) (exec "true && false || true")
        assertEqual "true && false || false" (Ret (B False) [] []) (exec "true && false || false")
        assertEqual "true && 5 || true" (RuntimeError "5 is not a bool" [] []) (exec "true && 5 || true")
        assertEqual "5 && true || true" (RuntimeError "5 is not a bool" [] []) (exec "5 && true || true")
        assertEqual "5 && 5 || 6" (RuntimeError "5 is not a bool" [] []) (exec "5 && 5 || 6")
        assertEqual "true || false && true" (Ret (B True) [] []) (exec "true || false && true"),
    testCase "|| + !: " $
     do
      assertEqual "!true || false" (Ret (B False) [] []) (exec "!true || false")
      assertEqual "!false || !false" (Ret (B True) [] []) (exec "!false || !false")
      assertEqual "false || !true" (Ret (B False) [] []) (exec "false || !true")
      assertEqual "false || !3.0" (RuntimeError "3.0 is not a bool" [] []) (exec "false || !3.0")
      assertEqual "false ||" (ParseError) (exec "false ||")
      assertEqual "|| !false" (ParseError) (exec "|| !false"),
    testCase "&& + !: " $
      do
        assertEqual "!true && false" (Ret (B False) [] []) (exec "!true && false")
        assertEqual "!false && !true" (Ret (B False) [] []) (exec "!false && !true")
        assertEqual "false && !true" (Ret (B False) [] []) (exec "false && !true")
        assertEqual "false && !3.0" (RuntimeError "3.0 is not a bool" [] []) (exec "false && !3.0")
        assertEqual "false &&" (ParseError) (exec "false &&")
        assertEqual "&& !false" (ParseError) (exec "&& !false")
    ],
  testGroup "Function Application"
    [
    testCase "Single Argument Lambdas: " $
      do
        assertEqual "(\\x -> x + 5) 5" (Ret (I 10) [] []) (exec "(\\x -> x + 5) 5")
        assertEqual "(\\x -> x + 5" (ParseError) (exec "(\\x -> x + 5")
        assertEqual
          "(\\x -> x + y) 5"
          (RuntimeError "Variable y is not defined or is not in scope" [] [UndefinedVarUse "y is not in scope"])
          (exec "(\\x -> x + y) 5")
        assertEqual "(\\x -> )" (ParseError) (exec "(\\x -> )")
        assertEqual "\\ -> x + 5" (ParseError) (exec "\\ -> x + 5")
        assertEqual
          "(\\ x -> x + 5.5) 4"
          (RuntimeError "TypeMismatch: Cannot add integer x and float 5.5" [] [])
          (exec "(\\ x -> x + 5.5) 4")
        assertEqual
          "(\\ x -> 5.5 + x) 4"
          (RuntimeError "TypeMismatch: Cannot add float 5.5 and integer x" [] [])
          (exec "(\\ x -> 5.5 + x) 4"),
    testCase "Multi-Argument Lambdas: " $
      do
        assertEqual "(\\ x y z -> x + y + z) 1 2 3" (Ret (I 6) [] []) (exec "(\\ x y z -> x + y + z) 1 2 3")
        assertEqual "(\\ x y z -> x + y + z) 1.0 2.0 3.0" (Ret (F 6.0) [] []) (exec "(\\ x y z -> x + y + z) 1.0 2.0 3.0")
        assertEqual "(\\ x y z -> x:y:z) 1 2 3" (Ret (Ls [I 1, I 2, I 3]) [] []) (exec "(\\ x y z -> x:y:z) 1 2 3")
        assertEqual
          "(\\ x y z -> x + y + z + a) 1 2 3"
          (RuntimeError "Variable a is not defined or is not in scope" [] [UndefinedVarUse "a is not in scope"])
          (exec "(\\ x y z -> x + y + z + a) 1 2 3")
        assertEqual "\\ x y -> " (ParseError) (exec "\\ x y -> ")
        assertEqual "\\ x 5 -> x + 5" (ParseError) (exec "\\ x 5 -> x + 5"),
    testCase "Applying lambdas to lambdas" $
      do
        assertEqual "(\\ x -> x) (\\ x -> x)" (Ret (I 5) [] []) (exec "(\\ x -> x) (\\ x -> x) 5")
        assertEqual "(\\ x -> x) (\\ x -> x) 5 5" (RuntimeError "5 is not a function" [] []) (exec "(\\ x -> x) (\\ x -> x) 5 5")
        assertEqual "(\\ x -> x) (\\ x y -> x + y) 5 5" (Ret (I 10) [] []) (exec "(\\ x -> x) (\\ x y -> x + y) 5 5"),
    testCase "Nested function application" $
      do
        assertEqual
          "(\\ x y -> x y) (\\ x -> x + 10) 10"
          (Ret (I 20) [] [])
          (exec "(\\ x y -> x y) (\\ x -> x + 10) 10")
        assertEqual
          "(\\ x y -> x y) 10 (\\ x -> x + 10)"
          (RuntimeError "10 is not a function" [] [])
          (exec "(\\ x y -> x y) 10 (\\ x -> x + 10)")
        assertEqual
          "(\\ x y -> x y 10 (\\ x -> x + 10)"
          (ParseError)
          (exec "(\\ x y -> x y 10 (\\ x -> x + 10)"),
    testCase "Erroneous Function Application" $
      do
        assertEqual "5 5" (RuntimeError "5 is not a function" [] []) (exec "5 5")
        assertEqual "(5) 5" (RuntimeError "5 is not a function" [] []) (exec "(5) 5")
        assertEqual "'c' 5" (RuntimeError "'c' is not a function" [] []) (exec "'c' 5")
        assertEqual "\"yo dawg\" 5" (RuntimeError "\"yo dawg\" is not a function" [] []) (exec "\"yo dawg\" 5")
        assertEqual "[1,2,3] 5" (RuntimeError "[1,2,3] is not a function" [] []) (exec "[1,2,3] 5")
    ],
  testGroup "Let Expressions"
    [
    testCase "Single definition let expressions: " $
      do
        assertEqual "let x = 5 in x + 5" (Ret (I 10) [] []) (exec "let x = 5 in x + 5")
        assertEqual
          "let x = 10 in x' + 10"
          (RuntimeError "Variable x' is not defined or is not in scope" [] [UndefinedVarUse "x' is not in scope"])
          (exec "let x = 10 in x' + 10")
        assertEqual "let x = 5 i x + 5" (ParseError) (exec "let x = 5 i x + 5")
        assertEqual "let = 5 in x + 5" (ParseError) (exec "let = 5 in x + 5")
        assertEqual
          "let x = 5 in y"
          (RuntimeError "Variable y is not defined or is not in scope" [] [UndefinedVarUse "y is not in scope"])
          (exec "let x = 5 in y"),
    testCase "Multi-definition let expressions: " $
      do
        assertEqual
          "let x = 5, y = 5, z = 5 in x + y + z"
          (Ret (I 15) [] [])
          (exec "let x = 5, y = 5, z = 5 in x + y + z")
        assertEqual
          "let x = 5 y = 5 z = 5 in x + y + z"
          (ParseError)
          (exec "let x = 5 y = 5 z = 5 in x + y + z")
        assertEqual
          "let x = 5, y = x, z = y in x + y + z"
          (Ret (I 15) [] [])
          (exec "let x = 5, y = x, z = y in x + y + z")
        assertEqual
          "let x = 5, y = x, z = y, in x + y + z"
          (ParseError)
          (exec "let x = 5, y = x, z = y, in x + y + z")
        assertEqual
          "let x = 4, y = z, z = 5 in x + y + z"
          (RuntimeError "Variable z is not defined or is not in scope" [] [UndefinedVarUse "z is not in scope"])
          (exec "let x = 4, y = z, z = 5 in x + y + z")
        assertEqual
          "let x = \\x -> x + 5, y = 5 in x y"
          (Ret (I 10) [] [])
          (exec "let x = \\x -> x + 5, y = 5 in x y")
    ],
  testGroup "Print Expressions"
    [
    testCase "Atomic print expressions: " $
      do
        assertEqual "print(10)" (Ret (I 10) ["10"] []) (exec "print(10)")
        assertEqual
          "print(\"yo dawg\")"
          (Ret (S "yo dawg") ["\"yo dawg\""] [])
          (exec "print(\"yo dawg\")")
        assertEqual "print(10" (ParseError) (exec "print(10")
        assertEqual
          "print(x)"
          (RuntimeError "Variable x is not defined or is not in scope" [] [UndefinedVarUse "x is not in scope"])
          (exec "print(x)")
        assertEqual
          "print x"
          (RuntimeError "Variable print is not defined or is not in scope" [] [(UndefinedVarUse "print is not in scope"), (UndefinedVarUse "x is not in scope")])
          (exec "print x")
        assertEqual "print(10) + print(5)" (Ret (I 15) ["10", "5"] []) (exec "print(10) + print(5)"),
    testCase "Print expressions inside functions: " $
      do
        assertEqual "(\\ x -> print(x)) 5" (Ret (I 5) ["5"] []) (exec "(\\ x -> print(x)) 5")
        assertEqual "(\\ x -> print(x) + print(x)) 5" (Ret (I 10) ["5", "5"] []) (exec "(\\ x -> print(x) + print(x)) 5")
        assertEqual "(\\ x -> x) (\\ y -> print(y)) 'A'" (Ret (C 'A') ["'A'"] []) (exec "(\\ x -> x) (\\ y -> print(y)) 'A'"),
    testCase "Print expressions under function composition: " $
      do
        assertEqual
          "(\\ x -> print(x)) . (\\ y -> print(y)) 10"
          (Ret (I 10) ["10", "10"] [])
          (exec "(\\ x -> print(x)) . (\\ y -> print(y)) 10"),
    testCase "Print expressions with separators: " $
      do
        assertEqual
          "print(10); \"yo dawg\""
          (Ret (S "yo dawg") ["10"] [])
          (exec "print(10); \"yo dawg\"")
    ],
  testGroup "Function Composition"
    [
    testCase "Composing functions with compatible types: " $
      do
        assertEqual
          "(\\ x -> x + 5.5) . (\\ x -> x + 5.5) 4.0"
          (Ret (F 15.0) [] [])
          (exec "(\\ x -> x + 5.5) . (\\ x -> x + 5.5) 4.0")
        assertEqual
          "(\\x -> x + 1) . (\\x -> x + 1) . (\\x -> x + 1) 1"
          (Ret (I 4) [] [])
          (exec "(\\x -> x + 1) . (\\x -> x + 1) . (\\x -> x + 1) 1"),
    testCase "Composing functions with incompatible types: " $
      do
        assertEqual
          "(\\ x -> x + 5) . (\\ x -> x + 5.5) 4.0"
          (RuntimeError "TypeMismatch: Cannot add float x and integer 5" [] [])
          (exec "(\\ x -> x + 5) . (\\ x -> x + 5.5) 4.0"),
    testCase "Erroneous function composition: " $
      do
        assertEqual
          "(\\ x -> x + 5) . 10"
          (RuntimeError "10 is not a function" [] [])
          (exec "(\\x -> x + 5) . 10")
    ],
  testGroup "List Index Expressions"
    [
    testCase "List Index with valid index: " $
      do
        assertEqual "[1,2,3] !! 2" (Ret (I 3) [] []) (exec "[1,2,3] !! 2")
        assertEqual "(1:2:3) !! 2" (Ret (I 3) [] []) (exec "(1:2:3) !! 2"),
    testCase "List Index with invalid index: " $
      do
        assertEqual
          "[1,2,3] !! 10"
          (RuntimeError "Index too large. Index given: 10 but maximum is: 2" [] [])
          (exec "[1,2,3] !! 10"),
    testCase "List Index applied to non-list data type: " $
      do
        assertEqual
          "10 !! [1,2,3]"
          (RuntimeError "10 is not a list" [] [])
          (exec "10 !! [1,2,3]")
    ],
  testGroup "If expressions"
    [
    testCase "Valid If expressions: " $
      do
        assertEqual
          "if (5 > 5) then \"yo dawg\" else 50"
          (Ret (I 50) [] [])
          (exec "if (5 > 5) then \"yo dawg\" else 50")
        assertEqual
          "if () then 50 else 55"
          (ParseError)
          (exec "if () then 50 else 55"),
    testCase "Malformed If expressions: " $
      do
        assertEqual
          "if 5 then 50 else 55"
          (RuntimeError "Condition (5) must evaluate to a boolean" [] [])
          (exec "if 5 then 50 else 55")
    ]
  ]



