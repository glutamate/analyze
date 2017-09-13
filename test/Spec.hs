module Main where

import Test.Hspec
import Test.QuickCheck

import Analyze as Analyze
import Analyze.Series as Series

main :: IO ()
main = hspec $ do
    describe "A Series" $ do
        it "can be counted from a list" $ do
            property seriesCount


seriesCount :: [Int] -> Bool
seriesCount lst
  = (zip lst lst & Series.fromList & countValues) == length lst


