import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "something" $ do
        it "should pass" $ do
            property $ \x xs -> head (x:xs) == (x :: Int)
