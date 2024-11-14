import Test.Tasty

import qualified Test.Sort
import qualified Test.Unit

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Sort" Test.Sort.props
                , testGroup "Unit" Test.Unit.unitTests
                , testGroup "Prop" Test.Prop.propTests 
                ])