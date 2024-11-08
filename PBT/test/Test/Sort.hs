module Test.Sort where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Sort
import qualified Data.List as L

-- Range 
-- |-----------|-----------| 
-- |--------->>|<<---------| 
-- -100  --    0    --   100

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-100) 100)

genList :: Int -> Int -> Gen [Int]
genList minLength maxLength = Gen.list (Range.constant minLength maxLength) genInt

prop_sorted :: Property 
prop_sorted = property $ do 
  list <- forAll $ genList 1 100 
  let sorted = sort list 
  assert (isSorted sorted)

-- sort list is a permutation (= it can be sorted to the same result) of the list 
prop_perm :: Property
prop_perm = property $ do
  list <- forAll $ genList 1 100
  let sorted = sort list
  assert (sorted == L.sort list)

props :: [TestTree]
props =
  [ testProperty "The sorted list is ordered" prop_sorted
  , testProperty "The sorted list is a permutation of the original list" prop_perm
  ]