{-# LANGUAGE OverloadedStrings #-}
-- | Domain types built from the parsed Parquet columns.
module Types (Event (..), eventsFromFile) where

import           Control.Applicative (ZipList (..))
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text as T

import           Parquet.Reader

data Event = Event
  { evUser  :: Text
  , evMatch :: Text
  , evMap   :: Text
  , evX     :: Double
  , evY     :: Double      -- elevation in this dataset
  , evZ     :: Double
  , evTs    :: Int         -- unix seconds
  , evKind  :: Text        -- "Position", "Loot", "BotKill", ...
  } deriving Show

-- | Turn the column-major data into rows, ordered by time.
eventsFromFile :: ParquetFile -> Either String [Event]
eventsFromFile pf = do
  user  <- textCol "user_id"
  match <- textCol "match_id"
  mapId <- textCol "map_id"
  x     <- floatCol "x"
  y     <- floatCol "y"
  z     <- floatCol "z"
  ts    <- intCol "ts"
  kind  <- textCol "event"
  pure . sortOn evTs . getZipList $
    Event <$> ZipList user <*> ZipList match <*> ZipList mapId
          <*> ZipList x <*> ZipList y <*> ZipList z
          <*> ZipList ts <*> ZipList kind
  where
    col name = maybe (Left ("missing column: " ++ T.unpack name)) Right (lookup name (pfColumns pf))
    textCol n = col n >>= \c -> case c of
      TextCol v -> Right v
      _         -> Left (T.unpack n ++ ": expected a string column")
    floatCol n = col n >>= \c -> case c of
      FloatCol v  -> Right (map realToFrac v)
      DoubleCol v -> Right v
      _           -> Left (T.unpack n ++ ": expected a float column")
    intCol n = col n >>= \c -> case c of
      IntCol v -> Right v
      _        -> Left (T.unpack n ++ ": expected an integer column")

-- | Compact form sent to the browser (user/match/map are known from the index).
instance ToJSON Event where
  toJSON e = object
    [ "kind" .= evKind e, "x" .= evX e, "y" .= evY e, "z" .= evZ e, "ts" .= evTs e ]
