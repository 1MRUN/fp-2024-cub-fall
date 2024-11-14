module Test.Prop where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import HW.Compiler as C
import HW.Eval as E
import HW.StackMachine as S
import Expr

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 100)

genVar1 :: Gen String
genVar1 = Gen.element ["x", "y", "z", "w", "k", "m"]



genExpr :: Gen (E.Expr String)
genExpr =
  Gen.recursive
    Gen.choice
    [ E.Num <$> genInt,
      E.Var <$> genName
    ]
    [ E.Let <$> genName <*> genExpr <*> genExpr,
      E.Plus <$> genExpr <*> genExpr
    ]

propVarUndefined :: Property
propVarUndefined = property $ do
  expr <- forAll genExpr
  let comp = C.compile expr
  assert(E.execProgram comp E.initialState == Left (E.VarUndefined _)) 

propTests :: [TestTree]
propTests =
  [ testProperty "Undefined Variables" propVarUndefined
  ]