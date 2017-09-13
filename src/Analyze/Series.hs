module Analyze.Series where

import Data.Serialize

fromList :: (Serialize key, Serialize value)
         => [ (key, value) ]
         -> Series key value
fromList = undefined
