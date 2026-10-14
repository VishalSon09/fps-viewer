module Parquet.Util (leWord) where

import           Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import           Data.Word (Word64)

-- | Read up to 8 bytes at an offset as a little-endian unsigned number.
leWord :: BS.ByteString -> Int -> Int -> Word64
leWord bs off n =
  BS.foldr' (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0 (BS.take n (BS.drop off bs))
