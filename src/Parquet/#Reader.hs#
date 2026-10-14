{-# LANGUAGE OverloadedStrings #-}
-- | Minimal Parquet reader: flat REQUIRED columns, data page v1,
-- PLAIN / dictionary encodings, UNCOMPRESSED or SNAPPY.
module Parquet.Reader
  ( Column (..), ParquetFile (..)
  , readParquetFile, parseParquet, columnLength, columnTypeName
  ) where

import           Control.Monad (foldM, when)
import           Data.Bifunctor (first)
import           Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import           Data.Int (Int32, Int64)
import           Data.List (transpose)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8')
import           Data.Word (Word64)
import           GHC.Float (castWord32ToFloat, castWord64ToDouble)

import qualified Parquet.Snappy as Snappy
import           Parquet.Thrift
import           Parquet.Util (leWord)

data Column
  = TextCol [Text]
  | FloatCol [Float]
  | DoubleCol [Double]
  | IntCol [Int]
  deriving Show

data ParquetFile = ParquetFile
  { pfRows    :: Int
  , pfColumns :: [(Text, Column)]
  }

columnLength :: Column -> Int
columnLength (TextCol v)   = length v
columnLength (FloatCol v)  = length v
columnLength (DoubleCol v) = length v
columnLength (IntCol v)    = length v

columnTypeName :: Column -> String
columnTypeName (TextCol _)   = "string"
columnTypeName (FloatCol _)  = "float"
columnTypeName (DoubleCol _) = "double"
columnTypeName (IntCol _)    = "int"

readParquetFile :: FilePath -> IO (Either String ParquetFile)
readParquetFile path = parseParquet <$> BS.readFile path

--------------------------------------------------------------------------------
-- File structure
--------------------------------------------------------------------------------

data Leaf = Leaf { lName :: Text, lType :: Int }

parseParquet :: BS.ByteString -> Either String ParquetFile
parseParquet bs = do
  let n = BS.length bs
  when (n < 12 || BS.take 4 bs /= "PAR1" || BS.drop (n - 4) bs /= "PAR1") $
    Left "not a Parquet file (missing PAR1 magic bytes)"
  let footerLen   = fromIntegral (leWord bs (n - 8) 4) :: Int
      footerStart = n - 8 - footerLen
  when (footerStart < 4) $ Left "corrupt footer length"
  (meta, _) <- runParser structP (BS.drop footerStart bs)
  rows    <- getField 3 meta >>= asInt
  schema  <- getField 2 meta >>= asList
  leaves  <- traverse leafOf (drop 1 schema)          -- first element is the root
  groups  <- getField 4 meta >>= asList
  perRg   <- traverse (rowGroupCols bs leaves) groups
  cols    <- traverse concatCols (transpose perRg)
  let named = zip (map lName leaves) cols
  when (any ((/= rows) . columnLength . snd) named) $
    Left "column length does not match the file's row count"
  pure (ParquetFile rows named)

leafOf :: TValue -> Either String Leaf
leafOf el = do
  name <- getField 4 el >>= asBin >>= (first show . decodeUtf8')
  ty   <- getField 1 el >>= asInt
  let rep = fromMaybe 0 (optField 3 el >>= either (const Nothing) Just . asInt)
  when (isJust (optField 5 el)) $
    Left ("nested columns are not supported yet: " ++ T.unpack name)
  when (rep /= 0) $
    Left ("column " ++ T.unpack name ++ " is OPTIONAL/REPEATED; only REQUIRED columns are supported yet")
  pure (Leaf name ty)

rowGroupCols :: BS.ByteString -> [Leaf] -> TValue -> Either String [Column]
rowGroupCols file leaves rg = do
  chunks <- getField 1 rg >>= asList
  when (length chunks /= length leaves) $ Left "row group column count mismatch"
  sequence (zipWith (decodeChunk file) leaves chunks)

decodeChunk :: BS.ByteString -> Leaf -> TValue -> Either String Column
decodeChunk file leaf chunk = do
  cm      <- getField 3 chunk
  codec   <- getField 4 cm >>= asInt
  total   <- getField 7 cm >>= asInt
  dataOff <- getField 9 cm >>= asInt
  let dictOff = optField 11 cm >>= either (const Nothing) Just . asInt
      start   = case dictOff of
                  Just d | d > 0 -> d
                  _              -> dataOff
  when (start < 0 || start + total > BS.length file) $ Left "column chunk lies outside the file"
  cols <- pagesLoop codec (lType leaf) Nothing (BS.take total (BS.drop start file))
  concatCols cols

--------------------------------------------------------------------------------
-- Pages
--------------------------------------------------------------------------------

pagesLoop :: Int -> Int -> Maybe Column -> BS.ByteString -> Either String [Column]
pagesLoop codec ty dict bs
  | BS.null bs = Right []
  | otherwise = do
      (hdr, rest) <- runParser structP bs
      pageType <- getField 1 hdr >>= asInt
      compSize <- getField 3 hdr >>= asInt
      when (compSize < 0 || compSize > BS.length rest) $ Left "page size exceeds chunk"
      raw <- decompressBody codec (BS.take compSize rest)
      let rest' = BS.drop compSize rest
      case pageType of
        2 -> do                                   -- dictionary page
          dh <- getField 7 hdr
          nv <- getField 1 dh >>= asInt
          d  <- plainDecode ty nv raw
          pagesLoop codec ty (Just d) rest'
        0 -> do                                   -- data page v1
          dh  <- getField 5 hdr
          nv  <- getField 1 dh >>= asInt
          enc <- getField 2 dh >>= asInt
          col <- dataPage ty dict nv enc raw
          (col :) <$> pagesLoop codec ty dict rest'
        _ -> Left ("unsupported page type " ++ show pageType
                   ++ " (only v1 data pages and dictionary pages are supported)")

decompressBody :: Int -> BS.ByteString -> Either String BS.ByteString
decompressBody 0 b = Right b
decompressBody 1 b = Snappy.decompress b
decompressBody c _ = Left ("unsupported compression codec " ++ show c ++ " (only UNCOMPRESSED and SNAPPY)")

dataPage :: Int -> Maybe Column -> Int -> Int -> BS.ByteString -> Either String Column
dataPage ty dict nv enc raw
  | enc == 0 = plainDecode ty nv raw
  | enc == 2 || enc == 8 = case (dict, BS.uncons raw) of
      (Just d, Just (bw, body)) -> do
        idx <- rleHybrid (fromIntegral bw) nv body
        pick d idx
      (Nothing, _) -> Left "dictionary-encoded page without a dictionary"
      (_, Nothing) -> Left "empty dictionary-encoded page"
  | otherwise = Left ("unsupported page encoding " ++ show enc)

--------------------------------------------------------------------------------
-- Value decoding
--------------------------------------------------------------------------------

plainDecode :: Int -> Int -> BS.ByteString -> Either String Column
plainDecode ty n bs = case ty of
  1 -> IntCol . map (fromIntegral . (fromIntegral :: Word64 -> Int32)) <$> fixed 4
  2 -> IntCol . map (fromIntegral . (fromIntegral :: Word64 -> Int64)) <$> fixed 8
  4 -> FloatCol . map (castWord32ToFloat . fromIntegral) <$> fixed 4
  5 -> DoubleCol . map castWord64ToDouble <$> fixed 8
  6 -> TextCol <$> byteArrays n bs
  _ -> Left ("unsupported physical type " ++ show ty)
  where
    fixed w
      | BS.length bs < n * w = Left "truncated PLAIN page"
      | otherwise            = Right [leWord bs (i * w) w | i <- [0 .. n - 1]]

byteArrays :: Int -> BS.ByteString -> Either String [Text]
byteArrays 0 _ = Right []
byteArrays k s
  | BS.length s < 4 = Left "truncated byte-array value"
  | otherwise =
      let l    = fromIntegral (leWord s 0 4) :: Int
          body = BS.take l (BS.drop 4 s)
      in if BS.length body < l
           then Left "truncated byte-array value"
           else do
             t <- first show (decodeUtf8' body)
             (t :) <$> byteArrays (k - 1) (BS.drop (4 + l) s)

-- | RLE / bit-packed hybrid, as used for dictionary indices.
rleHybrid :: Int -> Int -> BS.ByteString -> Either String [Int]
rleHybrid bw = go
  where
    go need bs
      | need <= 0 = Right []
      | BS.null bs = Left "truncated RLE/bit-packed data"
      | otherwise = do
          (h64, rest) <- runParser varint bs
          let h = fromIntegral h64 :: Int
          if odd h
            then do
              let groups = h `shiftR` 1
                  cnt    = groups * 8
                  nbytes = groups * bw
                  vals   = unpackBits bw cnt (BS.take nbytes rest)
              (take need vals ++) <$> go (need - cnt) (BS.drop nbytes rest)
            else do
              let cnt = h `shiftR` 1
                  w   = (bw + 7) `div` 8
                  v   = fromIntegral (leWord rest 0 w) :: Int
              (replicate (min cnt need) v ++) <$> go (need - cnt) (BS.drop w rest)

unpackBits :: Int -> Int -> BS.ByteString -> [Int]
unpackBits bw cnt bs = [extract i | i <- [0 .. cnt - 1]]
  where
    mask = (1 `shiftL` bw) - 1 :: Word64
    extract i =
      let p       = i * bw
          byteIdx = p `shiftR` 3
          sh      = p .&. 7
          nb      = (sh + bw + 7) `shiftR` 3
      in fromIntegral ((leWord bs byteIdx nb `shiftR` sh) .&. mask)

pick :: Column -> [Int] -> Either String Column
pick (TextCol d)   ix = TextCol   <$> pickWith d ix
pick (FloatCol d)  ix = FloatCol  <$> pickWith d ix
pick (DoubleCol d) ix = DoubleCol <$> pickWith d ix
pick (IntCol d)    ix = IntCol    <$> pickWith d ix

pickWith :: [a] -> [Int] -> Either String [a]
pickWith d = traverse look
  where
    m = IM.fromList (zip [0 ..] d)
    look i = maybe (Left "dictionary index out of range") Right (IM.lookup i m)

appendCol :: Column -> Column -> Either String Column
appendCol (TextCol a)   (TextCol b)   = Right (TextCol (a ++ b))
appendCol (FloatCol a)  (FloatCol b)  = Right (FloatCol (a ++ b))
appendCol (DoubleCol a) (DoubleCol b) = Right (DoubleCol (a ++ b))
appendCol (IntCol a)    (IntCol b)    = Right (IntCol (a ++ b))
appendCol _ _                         = Left "column type mismatch between pages"

concatCols :: [Column] -> Either String Column
concatCols []     = Left "column chunk contains no data pages"
concatCols (c:cs) = foldM appendCol c cs
