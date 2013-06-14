{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Db (
    User(..)
  , Position(..)
  , createTables
  , savePosition
  , listPositions) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           GHC.Generics (Generic) 
import qualified Data.Text as T
import           Data.Typeable 
import           Database.SQLite.Simple

data User = User Int T.Text


-- The Position type is used to model a physical location 
-- sent by a Raspberry Pi to the database.  Note that we capture
-- some extra information such as the IMEI and IMSI of the device
-- making the request.  
data Position t =
  Position
  { latitude :: t
  , longitude :: t
  , displayString :: t 
  , arbitraryText :: t 
  , timestampUTC :: t 
  , imei :: t 
  , imsi :: t 
  , cpuID :: t  
  } deriving (Show, Functor, Generic, Typeable)


-- The following instances allow serialization / deserialization 
-- of the Position objects defined above into/out of JSON.  
instance FromJSON (Position T.Text)
instance ToJSON (Position T.Text) 

-- Instance for converting query results back into Position. 
instance FromRow (Position T.Text) where
  fromRow = Position <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field  


tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: Connection -> IO ()
createTables conn = do
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "positions"
  unless schemaCreated $
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE positions ("
                , "created TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "latitude TEXT, "
                , "longitude TEXT, "
                , "display_string TEXT, " 
                , "arbitrary_text TEXT, "  
                , "timestamp_utc TEXT, "  
                , "imei TEXT,"
                , "imsi TEXT PRIMARY KEY," 
                , "cpu_id TEXT)" 
                ])

-- | Retrieve a list containing all of the reported positions.  
listPositions :: Connection -> IO [Position T.Text]
listPositions conn = query_ conn "SELECT latitude,longitude,display_string,arbitrary_text,timestamp_utc,imei,imsi,cpu_id FROM positions" :: IO [Position T.Text]  


-- | Save a position that has been reported by a Raspberry Pi to the database.  
savePosition :: Connection -> Position T.Text-> IO (Position T.Text)
savePosition conn p = do
      execute conn "INSERT OR REPLACE INTO positions (latitude,longitude,display_string,arbitrary_text,timestamp_utc,imei,imsi,cpu_id) VALUES (?,?,?,?,?,?,?,? )"
        (latitude p, longitude p, displayString p, arbitraryText p, timestampUTC p, imei p, imsi p, cpuID p)
      rowId <- lastInsertRowId conn
      liftIO $ print $ show p 
      liftIO $ print $ show rowId 
      return p 

