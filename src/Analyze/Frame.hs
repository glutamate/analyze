module Analyze.Frame where

import Control.Monad
import Data.Vector.Mutable as MutableVector

import Analyze.Types
import Analyze.Series as Series

data Frame rowKey columnKey = Frame
    { frameKeys    :: IOVector columnKey
    , frameColumns :: IOVector (Series rowKey)
    }

ofColumns :: (Serializable columnValue)
          => [ (columnKey, [columnValue] ) ]
          -> IO (Frame Int columnKey)
ofColumns keyValues = do
    keysVector <- MutableVector.new (Prelude.length keyValues)
    colsVector <- MutableVector.new (Prelude.length keyValues)
    forM_ (indexed keyValues) $ \(i, (columnKey, column)) -> do
        colSeries <- Series.ofValues column
        MutableVector.write keysVector i columnKey
        MutableVector.write colsVector i colSeries
    return Frame
        { frameKeys    = keysVector
        , frameColumns = colsVector
        }
  where
    indexed = Prelude.zip [0..]
