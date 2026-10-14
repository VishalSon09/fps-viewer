-- | Minimal Thrift Compact Protocol decoder (enough for Parquet metadata).
module Parquet.Thrift
  ( TValue (..), Parser, runParser, varint, structP
  , getField, optField, asInt, asBin, asList
  ) where

import           Control.Monad (ap, replicateM)
import           Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import           Data.Int (Int8, Int64)
import           Data.Word (Word8, Word64)
import           GHC.Float (castWord64ToDouble)
import           Parquet.Util (leWord)

data TValue
  = TBool Bool
  | TInt Int
  | TDouble Double
  | TBin BS.ByteString
  | TList [TValue]
  | TStruct (IM.IntMap TValue)
  deriving Show

-- A tiny parser that also returns the unconsumed input.
newtype Parser a = Parser { runParser :: BS.ByteString -> Either String (a, BS.ByteString) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Left e       -> Left e
    Right (a, r) -> Right (f a, r)

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)
  (<*>)  = ap

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> case p s of
    Left e       -> Left e
    Right (a, r) -> runParser (f a) r

failP :: String -> Parser a
failP msg = Parser $ \_ -> Left msg

byte :: Parser Word8
byte = Parser $ \s -> case BS.uncons s of
  Nothing      -> Left "thrift: unexpected end of data"
  Just (b, r)  -> Right (b, r)

takeP :: Int -> Parser BS.ByteString
takeP n = Parser $ \s ->
  if n < 0 || BS.length s < n then Left "thrift: unexpected end of data"
  else Right (BS.take n s, BS.drop n s)

varint :: Parser Word64
varint = go 0 0
  where
    go sh acc = do
      b <- byte
      let acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` sh)
      if b .&. 0x80 /= 0
        then if sh > 63 then failP "thrift: varint too long" else go (sh + 7) acc'
        else pure acc'

zigzag :: Word64 -> Int
zigzag n = fromIntegral ((fromIntegral (n `shiftR` 1) :: Int64) `xor` negate (fromIntegral (n .&. 1)))

zzInt :: Parser TValue
zzInt = TInt . zigzag <$> varint

valueP :: Word8 -> Parser TValue
valueP ty = case ty of
  1  -> pure (TBool True)
  2  -> pure (TBool False)
  3  -> (\b -> TInt (fromIntegral (fromIntegral b :: Int8))) <$> byte
  4  -> zzInt
  5  -> zzInt
  6  -> zzInt
  7  -> (TDouble . castWord64ToDouble . (\b -> leWord b 0 8)) <$> takeP 8
  8  -> do n <- varint
           TBin <$> takeP (fromIntegral n)
  9  -> listP
  10 -> listP
  12 -> structP
  _  -> failP ("thrift: unsupported type " ++ show ty)

listP :: Parser TValue
listP = do
  h <- byte
  let et  = h .&. 15
      sz0 = fromIntegral (h `shiftR` 4) :: Int
  n <- if sz0 == 15 then fromIntegral <$> varint else pure sz0
  TList <$> replicateM n (elemP et)
  where
    elemP et | et == 1 || et == 2 = (\b -> TBool (b == 1)) <$> byte
             | otherwise          = valueP et

structP :: Parser TValue
structP = go 0 IM.empty
  where
    go lastId acc = do
      h <- byte
      if h == 0
        then pure (TStruct acc)
        else do
          let ty    = h .&. 15
              delta = fromIntegral (h `shiftR` 4) :: Int
          fid <- if delta /= 0 then pure (lastId + delta) else zigzag <$> varint
          v   <- valueP ty
          go fid (IM.insert fid v acc)

--------------------------------------------------------------------------------
-- Accessors
--------------------------------------------------------------------------------

getField :: Int -> TValue -> Either String TValue
getField k (TStruct m) = maybe (Left ("missing thrift field " ++ show k)) Right (IM.lookup k m)
getField k _           = Left ("expected struct while reading field " ++ show k)

optField :: Int -> TValue -> Maybe TValue
optField k (TStruct m) = IM.lookup k m
optField _ _           = Nothing

asInt :: TValue -> Either String Int
asInt (TInt n) = Right n
asInt v        = Left ("expected integer, got " ++ show v)

asBin :: TValue -> Either String BS.ByteString
asBin (TBin b) = Right b
asBin v        = Left ("expected binary, got " ++ show v)

asList :: TValue -> Either String [TValue]
asList (TList xs) = Right xs
asList v          = Left ("expected list, got " ++ show v)
