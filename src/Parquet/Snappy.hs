{-# LANGUAGE BangPatterns #-}
-- | Pure-Haskell Snappy (raw block format) decompressor.
module Parquet.Snappy (decompress) where

import           Control.Monad (forM_)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import           Data.IORef
import           Data.Word (Word8)
import           Foreign.Marshal.Utils (copyBytes)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (peekByteOff, pokeByteOff)
import           System.IO.Unsafe (unsafePerformIO)

decompress :: BS.ByteString -> Either String BS.ByteString
decompress src = do
  (outLen, hdrLen) <- preamble 0 0 0
  if outLen > 1073741824
    then Left "snappy: declared output size is unreasonably large"
    else unsafePerformIO $ do
      err <- newIORef Nothing
      bs <- BI.create outLen $ \out -> do
        r <- run outLen out hdrLen 0
        writeIORef err (either Just (const Nothing) r)
      e <- readIORef err
      pure (maybe (Right bs) Left e)
  where
    srcLen = BS.length src

    byteAt :: Int -> Int
    byteAt i | i >= 0 && i < srcLen = fromIntegral (BU.unsafeIndex src i)
             | otherwise            = 0

    -- little-endian integer of n bytes starting at i
    leInt :: Int -> Int -> Int
    leInt i n = sum [byteAt (i + k) `shiftL` (8 * k) | k <- [0 .. n - 1]]

    -- varint holding the uncompressed length
    preamble :: Int -> Int -> Int -> Either String (Int, Int)
    preamble !sh !acc !i
      | i >= srcLen = Left "snappy: truncated preamble"
      | sh > 35     = Left "snappy: bad preamble"
      | otherwise   =
          let b    = byteAt i
              acc' = acc .|. ((b .&. 0x7f) `shiftL` sh)
          in if b .&. 0x80 /= 0 then preamble (sh + 7) acc' (i + 1) else Right (acc', i + 1)

    run :: Int -> Ptr Word8 -> Int -> Int -> IO (Either String ())
    run outLen out = go
      where
        go !i !o
          | i >= srcLen = pure (if o == outLen then Right () else Left "snappy: output length mismatch")
          | otherwise =
              let t = byteAt i
              in case t .&. 3 of
                0 ->
                  let l = t `shiftR` 2
                      (len, i') = if l < 60 then (l + 1, i + 1)
                                  else let nb = l - 59 in (leInt (i + 1) nb + 1, i + 1 + nb)
                  in if i' + len > srcLen || o + len > outLen
                       then pure (Left "snappy: bad literal")
                       else do
                         BU.unsafeUseAsCString src $ \p ->
                           copyBytes (out `plusPtr` o) (castPtr p `plusPtr` i') len
                         go (i' + len) (o + len)
                1 | i + 2 > srcLen -> pure (Left "snappy: truncated copy")
                  | otherwise ->
                      backref (((t `shiftR` 2) .&. 7) + 4) (((t `shiftR` 5) `shiftL` 8) .|. byteAt (i + 1)) (i + 2) o
                2 | i + 3 > srcLen -> pure (Left "snappy: truncated copy")
                  | otherwise -> backref ((t `shiftR` 2) + 1) (leInt (i + 1) 2) (i + 3) o
                _ | i + 5 > srcLen -> pure (Left "snappy: truncated copy")
                  | otherwise -> backref ((t `shiftR` 2) + 1) (leInt (i + 1) 4) (i + 5) o

        backref len off next o
          | off <= 0 || off > o || o + len > outLen = pure (Left "snappy: bad back-reference")
          | otherwise = do
              -- byte by byte: copies may overlap their own output
              forM_ [0 .. len - 1] $ \k -> do
                b <- peekByteOff out (o + k - off) :: IO Word8
                pokeByteOff out (o + k) b
              go next (o + len)
