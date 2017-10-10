-- | Functions for working with CSV files.
module Analyze.Csv where

-- import           Analyze.Conversions        (projectRows)
import           Analyze.Frame
import           Control.Monad.Catch        (Exception, MonadThrow (..))
-- import qualified Data.Binary.Builder        as B
import qualified Data.ByteString.Lazy       as LBS
-- import qualified Data.ByteString.Lazy.Char8 as LBS8
-- import qualified Data.Csv                   as C
-- import qualified Data.Csv.Builder           as CB
import           Data.Text                  (Text)
-- import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Typeable              (Typeable)
-- import qualified Data.Vector                as V

-- | Exception to wrap Cassava error strings.
data CsvError = CsvError String deriving (Eq, Show, Typeable)
instance Exception CsvError

-- | Decode CSV bytes as an 'Frame' with a header row.
decodeWithHeader :: MonadThrow m => LBS.ByteString -> m (Frame Text Text)
decodeWithHeader = undefined

-- | Decode CSV bytes as an 'Frame' without a header row.
decodeWithoutHeader :: MonadThrow m => LBS.ByteString -> m (Frame Int Text)
decodeWithoutHeader = undefined

-- | Encode an 'Frame' as CSV bytes with a header row.
encodeWithHeader :: Frame Text Text -> LBS.ByteString
encodeWithHeader = undefined

-- | Encode an 'Frame' as CSV bytes without header row.
encodeWithoutHeader :: Frame k Text -> LBS.ByteString
encodeWithoutHeader = undefined


loadCSVFileWithHeader :: FilePath -> IO (Frame Text Text)
loadCSVFileWithHeader = undefined

loadCSVFileWithoutHeader :: FilePath -> IO (Frame Int Text)
loadCSVFileWithoutHeader = undefined

