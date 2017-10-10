-- | Simple structural conversions.
module Analyze.Conversions
  ( projectRow
  , projectRows
  ) where

import           Analyze.Common      (Data)
import           Analyze.Frame
import           Control.Monad.Catch (MonadThrow (..))
import           Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HM
import           Data.Vector         (Vector)
-- import qualified Data.Vector         as V

-- | Projects values out of the map according to the given key order.
projectRow :: (Data k, MonadThrow m) => Vector k -> HashMap k v -> m (Vector v)
projectRow = undefined

-- | Projects an 'Frame' out of many maps according to the given key order.
projectRows :: (Data k, MonadThrow m) => Vector k -> Vector (HashMap k v) -> m (Frame k v)
projectRows = undefined
