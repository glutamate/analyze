module Analyze.Frame where

-- | Creates a 'Frame' in the same way as 'Data.Map.fromList'.
-- Series values must be serialized to a 'ByteString' using
-- 'Series.serialize'.
--
-- @
--     myDataFrame :: Frame Text Text
--     myDataFrame = Frame.fromList
--        [ ("Name" , Series.serialize names)
--        , ("Price", Series.serialize prices)
--        ]
-- @
fromList :: (Serialize key)
         => [(key, Series key ByteString)]
         -> AnalyzeOperation (Frame key ByteString)
fromList = undefined


-- | Adds a column to a frame
--
-- @
-- myDataFrame
-- & addColumn "Date" dates
-- & addColumn "Zip code" zipCodes
-- @
addColumn :: (Serialize key, Serialize value)
          => key
          -> Series key value
          -> Frame key value
          -> AnalyzeOperation (Frame key value)
addColumn = undefined


-- | AnalyzeOperation the columns of the 'Frame' as a 'Series'
-- of serialized 'Series' indexed by the same key.
columns :: (Serialize key, Serialize value)
        => Frame key value
        -> AnalyzeOperation (Series key (Series ByteString ByteString))
columns = undefined


-- | Total number of columns, including the ones with no value
countColumns :: (Serialize key, Serialize value)
             => Frame key value
             -> AnalyzeOperation Int
countColumns = undefined


-- | Total number of rows
countRows :: (Serialize key, Serialize value)
          => Frame key value
          -> AnalyzeOperation Int
countRows = undefined


dropColumn :: (Serialize key, Serialize value)
           => key
           -> Frame key value
           -> AnalyzeOperation (Frame key value)
dropColumn = undefined
