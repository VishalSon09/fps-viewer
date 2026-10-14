{-# LANGUAGE OverloadedStrings #-}
-- | Scans the player_data folder tree and keeps a small in-memory index.
module Library
  ( MatchEntry (..), MapConfig (..), mapConfigs
  , scanLibrary, findMinimaps, loadEvents
  ) where

import           Control.Monad (filterM, forM)
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Char (toLower)
import           Data.List (isPrefixOf, isSuffixOf, sort, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath (takeExtension, (</>))
import           System.IO (hPutStrLn, stderr)

import           Parquet.Reader (readParquetFile)
import           Types

-- | One match file. Events are NOT kept in memory; they are re-read on demand.
data MatchEntry = MatchEntry
  { meId     :: !Int
  , meMap    :: !Text
  , meDate   :: !Text
  , mePlayer :: !Text
  , meMatch  :: !Text
  , mePath   :: !FilePath
  , meKinds  :: !(M.Map Text Int)   -- event type -> count
  }

instance ToJSON MatchEntry where
  toJSON e = object
    [ "id" .= meId e, "map" .= meMap e, "date" .= meDate e
    , "player" .= mePlayer e, "match" .= meMatch e, "kinds" .= meKinds e ]

-- | World -> minimap conversion parameters (minimaps are 1024x1024).
data MapConfig = MapConfig
  { mcId    :: Text
  , mcScale :: Double
  , mcOx    :: Double
  , mcOz    :: Double
  }

mapConfigs :: [MapConfig]
mapConfigs =
  [ MapConfig "AmbroseValley" 900  (-370) (-473)
  , MapConfig "GrandRift"     581  (-290) (-290)
  , MapConfig "Lockdown"      1000 (-500) (-500)
  ]

-- | root/<date folder>/<user>_<match>.nakama*  (the "minimaps" folder is skipped)
scanLibrary :: FilePath -> IO [MatchEntry]
scanLibrary root = do
  names <- listDirectory root
  dirs  <- filterM (doesDirectoryExist . (root </>)) (filter (/= "minimaps") names)
  raw <- fmap concat . forM (sortOn dateKey dirs) $ \d -> do
    files <- sort <$> listDirectory (root </> d)
    fmap catMaybes . forM (filter isNakama files) $ \f ->
      entryFor d (root </> d </> f) f
  pure (zipWith (\i e -> e { meId = i }) [0 ..] raw)
  where
    isNakama f = ".nakama" `isPrefixOf` takeExtension f

entryFor :: String -> FilePath -> String -> IO (Maybe MatchEntry)
entryFor date path file = do
  r <- readParquetFile path
  case r >>= eventsFromFile of
    Left err -> hPutStrLn stderr ("skipping " ++ path ++ ": " ++ err) >> pure Nothing
    Right [] -> pure Nothing
    Right evs@(e : _) -> do
      let stem             = T.pack (takeWhile (/= '.') file)
          (player, rest)   = T.breakOn "_" stem
      pure . Just $ MatchEntry
        { meId     = 0
        , meMap    = evMap e
        , meDate   = T.pack date
        , mePlayer = player
        , meMatch  = T.drop 1 rest
        , mePath   = path
        , meKinds  = M.fromListWith (+) [(evKind x, 1) | x <- evs]
        }

-- | Sort key for folder names like "February_10".
dateKey :: String -> (Int, Int)
dateKey s = (month, day)
  where
    (m, rest) = break (== '_') s
    day :: Int
    day = case reads (drop 1 rest) of
            [(n, "")] -> n
            _         -> 0
    month :: Int
    month = fromMaybe 99 (lookup (map toLower m) (zip months [1 ..]))
    months = [ "january", "february", "march", "april", "may", "june", "july"
             , "august", "september", "october", "november", "december" ]

-- | minimaps/<MapId>_Minimap.(png|jpg|jpeg)  ->  MapId => file path
findMinimaps :: FilePath -> IO (M.Map Text FilePath)
findMinimaps root = do
  let dir = root </> "minimaps"
  ok <- doesDirectoryExist dir
  if not ok
    then pure M.empty
    else do
      fs <- listDirectory dir
      pure (M.fromList (mapMaybe (\f -> (\b -> (T.pack b, dir </> f)) <$> minimapId f) fs))
  where
    minimapId f = case [ take (length f - length suf) f
                       | ext <- [".png", ".jpg", ".jpeg"]
                       , let suf = "_Minimap" ++ ext
                       , suf `isSuffixOf` f ] of
                    (b : _) -> Just b
                    []      -> Nothing

loadEvents :: MatchEntry -> IO (Either String [Event])
loadEvents e = (>>= eventsFromFile) <$> readParquetFile (mePath e)
