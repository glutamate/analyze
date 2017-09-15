{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Analyze.Types where

import Data.Text as Text

type Serializable a = (Read a, Show a, Serializable' a)


-- | Represents all values that can be stored in a
-- 'Series' or 'Frame'
class Serializable' a where
    serialize :: a -> Text
    deserialize :: Text -> a
-- This is a really inefficient way to represent serialization
-- might be better to (de)serialize from/to 'Text' directly.

-- instance Serializable' [Char] where
--     serialize = Text.pack
--     deserialize = Text.unpack

instance (Read a, Show a) => Serializable' a where
    serialize = Text.pack . show
    deserialize = read . Text.unpack
