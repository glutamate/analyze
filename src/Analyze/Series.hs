{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Analyze.Series where

import Control.Monad
import Data.Text as Text
import Data.Vector.Mutable as MutableVector
import System.IO.Unsafe

import Analyze.Types


data Series key = Series
    { seriesKeys   :: IOVector key
    , seriesValues :: IOVector Text
    }


instance (Show key) => Show (Series key) where
    show series = "Keys\t\tValues\n"
                ++ Prelude.concatMap showKeyValue kvPairs
      where
        showKeyValue (k, v) = show k ++ "\t\t" ++ show v ++ "\n"
        keys = seriesKeys series
        values = seriesValues series
        kvPairs = Analyze.Series.zip keys values


ofValues :: (Serializable value)
         => [value]
         -> IO (Series Int)
ofValues values = do
    sKeys   :: IOVector Int <- MutableVector.new (Prelude.length values)
    sValues :: IOVector Text <- MutableVector.new (Prelude.length values)
    let result = Series sKeys sValues
    mapM_ (uncurry $ writeWithIndex result) (indexed values)
    return $ Series sKeys sValues
  where
    writeWithIndex series i value = writeTo i i value series
    indexed lst = Prelude.zip [0 .. Prelude.length lst] lst

writeTo :: (Serializable value)
        => Int
        -> key
        -> value
        -> Series key
        -> IO ()
writeTo i k v s = do
    write (seriesKeys s) i k
    write (seriesValues s) i (Text.pack.show $ v)

zip :: IOVector a
    -> IOVector b
    -> [ (a, b) ]
zip xs ys = unsafePerformIO $ ioZip xs ys

ioZip :: IOVector a
      -> IOVector b
      -> IO [ (a, b) ]
ioZip xs ys = do
    let l = min (MutableVector.length xs) (MutableVector.length ys)
    forM [1..l] $ \i -> do
        x <- MutableVector.read xs i
        y <- MutableVector.read ys i
        return (x, y)
