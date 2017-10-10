{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Core frame types and functions
module Analyze.Frame where

-- import           Control.Monad       (join)
import           Control.Monad.Catch (MonadThrow (..))

-- import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
--import qualified Data.Text           as Text
import           Data.Text           (Text)
import qualified Data.Vector         as V
import           Data.Vector         (Vector)

import           Analyze.Common
-- import           Analyze.Decoding    (Decoder (..), decoderKeys, runDecoder)

-- | In-memory row-oriented frame with columns named by `k` and values by `v`
data Frame r c = Frame
  { -- | Ordered vector of row names
    _frameRowKeys   :: !(Vector r)
  , -- | Ordered vector of column names
    _frameColumnKeys   :: !(Vector c)
  , -- | Quick lookup from column name to column index
    _frameLookup :: !(HashMap c Int)
  , -- | Vector of rows. Each element should be the length of number of columns.
    _frameData   :: !(Vector (Vector Text))
  } deriving (Eq, Show)

-- | Prettier alias for getting the keys of an 'Frame'
keys :: Frame r c -> Vector c
keys = _frameColumnKeys

-- | Prettier alias for getting the data matrix of an 'Frame'
frameData :: Frame r c -> Vector (Vector Text)
frameData = _frameData

-- | An empty frame with no rows or columns
empty :: Frame r c
empty = Frame V.empty V.empty HM.empty V.empty

-- | Build an 'Frame' from an 'FrameUpdate'.
--   Throws on duplicate keys.
-- fromUpdate :: (Data r, MonadThrow m) => FrameUpdate r c -> m (Frame r c)
-- fromUpdate (FrameUpdate rs cs vs) = checkForDupes rs >> pure (Frame rs cs (makeLookup rs) vs)

-- | Build an 'FrameUpdate' from an 'Frame'
-- toUpdate :: Data r => Frame r c -> FrameUpdate r c
-- toUpdate (Frame rs cs _ vs) = FrameUpdate rs cs vs

-- | Number of columns in an 'Frame'
numCols :: Frame r c -> Int
numCols (Frame _ cs _ _) = V.length cs

-- | Number of rows in an 'Frame'
numRows :: Frame r c -> Int
numRows (Frame rs _ _ _) = V.length rs

-- | Project to the given column
-- col :: (Data r, MonadThrow m) => r -> Frame r c -> m (Vector Text)
-- col k (Frame _ _ look vs) = V.mapM (\v -> runLookup look v k) vs

-- | Decode by row. Each element of the returned vector may fail on decoding error
--   so flatten manually or use 'flatDecode'.
-- decode :: (Data r, MonadThrow m) => Decoder m r c a -> Frame r c -> m (Vector (m a))
-- decode decoder (Frame ks _ look vs) = checkSubset required keySet >> pure decoded
--   where
--     keySet = HS.fromList (V.toList ks)
--     required = decoderKeys decoder
--     decoded = runDecoder decoder . runLookup look <$> vs

-- | An auto-flattened version of 'decode'.
-- flatDecode :: (Data r, MonadThrow m) => Decoder m r c a -> Frame r c -> m (Vector a)
-- flatDecode decoder frame = join $ sequence <$> decode decoder frame

-- | Filter an 'Frame' by row
-- filter :: Data r => FrameFilter r Text -> Frame r c -> Frame r c
-- filter p (Frame rs cs look vs) = Frame rs cs look vs'
--   where
--     vs' = V.ifilter (p rs look) vs

-- | Update row-wise, adding or replacing values per-column.
--   Retains the existing column order, appending new columns.
--   Throws on row length mismatch or duplicate columns in the update.
-- update :: (Data r, MonadThrow m) => FrameUpdate r c -> Frame r c -> m (Frame r c)
-- update (FrameUpdate urs ucs uvs) (Frame frs fcs _ fvs) = do
--   let fSize = V.length fvs
--       uSize = V.length uvs
--   if fSize /= uSize
--     then throwM (RowSizeMismatch fSize uSize)
--     else do
--       checkForDupes urs
--       let kis = mergeKeys frs urs
--           ks' = (\(k, _, _) -> k) <$> kis
--           look' = makeLookup ks'
--           vs' = V.zipWith (runIndexedLookup kis) fvs uvs
--       return (Frame ks' fcs look' vs')

-- | Split columns in an 'Frame' by a predicate.
-- splitCols :: Data r => (r -> Bool) -> Frame r c -> (Frame r c, Frame r c)
-- splitCols p (Frame rs cs look vs) = (Frame keepKs cs keepLook keepVs, Frame dropKs cs dropLook dropVs)
--   where
--     (keepKs, dropKs) = V.partition p rs
--     keepLook = makeLookup keepKs
--     keepVs = reorder keepKs look <$> vs
--     dropLook = makeLookup dropKs
--     dropVs = reorder dropKs look <$> vs

-- | Drop columns in an 'Frame' by a predicate.
-- dropCols :: Data r => (r -> Bool) -> Frame r c -> Frame r c
-- dropCols p frame = snd (splitCols p frame)

-- | Keep columns in an 'Frame' by a predicate.
-- keepCols :: Data r => (r -> Bool) -> Frame r c -> Frame r c
-- keepCols p frame = fst (splitCols p frame)

-- | Appends rows to an 'Frame', retaining column order of the first.
--   Throws on column mismatch.
-- appendRows :: (Data r, MonadThrow m) => Frame r c -> Frame r c -> m (Frame r c)
-- appendRows (Frame ks0 cs look0 vs0) (Frame ks1 cs' look1 vs1) = do
--   checkReorder ks0 ks1
--   let vs1' = reorder ks0 look1 vs1
--   return (Frame ks0 cs look0 (vs0 V.++ vs1'))

-- | Appends columns to an 'Frame', retaining column order of the first.
-- extendCols :: (Data r, MonadThrow m) => Frame r c -> Frame r c -> m (Frame r c)
-- extendCols f g = update (toUpdate g) f

-- | Takes first 'n' rows of an 'Frame'.
takeRows :: Int -> Frame r c -> Frame r c
takeRows n (Frame ks cs look vs) = Frame ks cs look (V.take n vs)

-- | Adds a 'Vector' column to the 'Frame'
addColumn :: (Data r, MonadThrow m) => Frame r c -> c -> Vector Text -> m (Frame r c)
addColumn = undefined
