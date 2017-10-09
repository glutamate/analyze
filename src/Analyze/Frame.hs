{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Core frame types and functions
module Analyze.Frame where

import           Analyze.Common
import           Analyze.Decoding    (Decoder (..), decoderKeys, runDecoder)
import           Control.Monad       (join)
import           Control.Monad.Catch (MonadThrow (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- | In-memory row-oriented frame with columns named by `k` and values by `v`
data Frame k v = Frame
  { -- | Ordered vector of column names
    _rframeKeys   :: !(Vector k)
  , -- | Quick lookup from column name to column index
    _rframeLookup :: !(HashMap k Int)
  , -- | Vector of rows. Each element should be the length of number of columns.
    _rframeData   :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor)

-- | A simpler 'Frame' for updates
data FrameUpdate k v = FrameUpdate
  { -- | Ordered vector of column names
    _rframeUpdateKeys :: !(Vector k)
  , -- | Vector of rows.
    _rframeUpdateData :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor)

-- | Alias for a function to be applied to each row
type FrameMap k v a = Vector k -> HashMap k Int -> Int -> Vector v -> a

-- | Alias for a row filter
type FrameFilter k v = FrameMap k v Bool

-- | Prettier alias for getting the keys of an 'Frame'
rframeKeys :: Frame k v -> Vector k
rframeKeys = _rframeKeys

-- | Prettier alias for getting the data matrix of an 'Frame'
rframeData :: Frame k v -> Vector (Vector v)
rframeData = _rframeData

-- | An empty frame with no rows or columns
empty :: Frame k v
empty = Frame V.empty HM.empty V.empty

-- | Build an 'Frame' from an 'FrameUpdate'.
--   Throws on duplicate keys.
fromUpdate :: (Data k, MonadThrow m) => FrameUpdate k v -> m (Frame k v)
fromUpdate (FrameUpdate ks vs) = checkForDupes ks >> pure (Frame ks (makeLookup ks) vs)

-- | Build an 'FrameUpdate' from an 'Frame'
toUpdate :: Data k => Frame k v -> FrameUpdate k v
toUpdate (Frame ks _ vs) = FrameUpdate ks vs

-- | Number of columns in an 'Frame'
numCols :: Frame k v -> Int
numCols (Frame ks _ _) = V.length ks

-- | Number of rows in an 'Frame'
numRows :: Frame k v -> Int
numRows (Frame _ _ vs) = V.length vs

-- | Project to the given column
col :: (Data k, MonadThrow m) => k -> Frame k v -> m (Vector v)
col k (Frame _ look vs) = V.mapM (\v -> runLookup look v k) vs

-- | Decode by row. Each element of the returned vector may fail on decoding error
--   so flatten manually or use 'flatDecode'.
decode :: (Data k, MonadThrow m) => Decoder m k v a -> Frame k v -> m (Vector (m a))
decode decoder (Frame ks look vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . runLookup look <$> vs

-- | An auto-flattened version of 'decode'.
flatDecode :: (Data k, MonadThrow m) => Decoder m k v a -> Frame k v -> m (Vector a)
flatDecode decoder rframe = join $ sequence <$> decode decoder rframe

-- | Filter an 'Frame' by row
filter :: Data k => FrameFilter k v -> Frame k v -> Frame k v
filter p (Frame ks look vs) = Frame ks look vs'
  where
    vs' = V.ifilter (p ks look) vs

-- | Update row-wise, adding or replacing values per-column.
--   Retains the existing column order, appending new columns.
--   Throws on row length mismatch or duplicate columns in the update.
update :: (Data k, MonadThrow m) => FrameUpdate k v -> Frame k v -> m (Frame k v)
update (FrameUpdate uks uvs) (Frame fks _ fvs) = do
  let fSize = V.length fvs
      uSize = V.length uvs
  if fSize /= uSize
    then throwM (RowSizeMismatch fSize uSize)
    else do
      checkForDupes uks
      let kis = mergeKeys fks uks
          ks' = (\(k, _, _) -> k) <$> kis
          look' = makeLookup ks'
          vs' = V.zipWith (runIndexedLookup kis) fvs uvs
      return (Frame ks' look' vs')

-- | Split columns in an 'Frame' by a predicate.
splitCols :: Data k => (k -> Bool) -> Frame k v -> (Frame k v, Frame k v)
splitCols p (Frame ks look vs) = (Frame keepKs keepLook keepVs, Frame dropKs dropLook dropVs)
  where
    (keepKs, dropKs) = V.partition p ks
    keepLook = makeLookup keepKs
    keepVs = reorder keepKs look <$> vs
    dropLook = makeLookup dropKs
    dropVs = reorder dropKs look <$> vs

-- | Drop columns in an 'Frame' by a predicate.
dropCols :: Data k => (k -> Bool) -> Frame k v -> Frame k v
dropCols p frame = snd (splitCols p frame)

-- | Keep columns in an 'Frame' by a predicate.
keepCols :: Data k => (k -> Bool) -> Frame k v -> Frame k v
keepCols p frame = fst (splitCols p frame)

-- | Appends rows to an 'Frame', retaining column order of the first.
--   Throws on column mismatch.
appendRows :: (Data k, MonadThrow m) => Frame k v -> Frame k v -> m (Frame k v)
appendRows (Frame ks0 look0 vs0) (Frame ks1 look1 vs1) = do
  checkReorder ks0 ks1
  let vs1' = reorder ks0 look1 vs1
  return (Frame ks0 look0 (vs0 V.++ vs1'))

-- | Appends columns to an 'Frame', retaining column order of the first.
extendCols :: (Data k, MonadThrow m) => Frame k v -> Frame k v -> m (Frame k v)
extendCols f g = update (toUpdate g) f

-- | Takes first 'n' rows of an 'Frame'.
takeRows :: Int -> Frame k v -> Frame k v
takeRows n (Frame ks look vs) = Frame ks look (V.take n vs)

-- | Adds a 'Vector' column to the 'Frame'
addColumn :: (Data k, MonadThrow m) => Frame k v -> k -> Vector v -> m (Frame k v)
addColumn rf name v = do
  c <- newFrameColumn name $ V.singleton <$> v
  extendCols rf c
 where
  newFrameColumn rfName = fromUpdate . FrameUpdate (V.singleton rfName)
