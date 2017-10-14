{-# LANGUAGE GADTs #-}

module Analyze.New where

import Data.Typeable
import Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable

class (Functor f, Foldable f) => FrameContainer f where
  zipWithFC :: (a->b->c) -> f a -> f b -> f c
  mzipWithFC :: (a->b->Maybe c) -> f a -> f b -> f c

instance FrameContainer [] where
  zipWithFC = zipWith
  mzipWithFC f [] _ = []
  mzipWithFC f _ [] = []
  mzipWithFC f (x:xs) (y:ys) = case f x y of
    Nothing -> mzipWithFC f xs ys
    Just z -> z:mzipWithFC f xs ys

data Column f where
  Col :: Typeable a => f a -> Column f

data Frame f k = Frame
  {  frameData :: HashMap k (Column f)
  }

instance Monoid (Frame f k) where
  mempty = Frame (HM.empty)
  mappend = undefined

mapCol :: (Eq k, Hashable k, Typeable a, Typeable b, Show k, Functor f)
       => k -> (a -> b) -> Frame f k -> Frame f k
mapCol k f fr@(Frame hm) =
  let c = col k fr
      nc = Col $ fmap f c
  in Frame $ HM.insert k nc hm

filterCol :: (Eq k, Hashable k, Typeable a, Show k, FrameContainer f)
  => k -> (a -> Bool) -> Frame f k -> Frame f k
filterCol k f fr@(Frame hm) =
  let c = col k fr
      trues = fmap f c
      w True x = Just x
      w False _ = Nothing
  in Frame $ HM.map (\(Col g) -> Col $ mzipWithFC w trues g) hm


col :: (Eq k, Hashable k, Typeable a, Show k, Functor f)
    => k -> Frame f k -> f a
col k fr = case HM.lookup k (frameData fr) of
  Nothing -> error $ "no such key" ++ show k
  Just (Col v) -> case gcast v of
    Nothing -> error "cast failed"
    Just v1 -> v1

myFrame :: Frame [] String
myFrame = Frame (HM.fromList [("b", Col [1::Int,2,3]), ("a", Col [4::Double,5,6])])

myCol :: [Int]
myCol = col "b" myFrame

myCol1 :: [Int]
myCol1 = col "b" $ mapCol "b" (+(1::Int)) $ filterCol "b" (odd::Int -> Bool) myFrame

{- todo

- col should be more permissible, e.g. convert Int to Integer
- monadthrow instead of error

-}