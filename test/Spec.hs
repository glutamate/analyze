module Main where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Tests" $ do
        it "passes" $ do
            property $ \x xs -> head (x:xs) == (x :: Int)


