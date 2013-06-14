{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-} 
------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent (withMVar)
import           Control.Monad.Trans (liftIO)
-- import           Control.Error.Safe (tryJust)
import           Control.Lens ((^#))
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Database.SQLite.Simple as S
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Extras.JSON
import           Text.Regex 
import           Heist()
------------------------------------------------------------------------------
import           Application
import qualified Db
-- import           Util

type H = Handler App App


-- | Run an IO action with an SQLite connection
withDb :: (S.Connection -> IO a) -> H a
withDb action =
  withTop db . withSqlite $ \conn -> action conn


filterText :: T.Text -> T.Text 
filterText s = T.pack $ subRegex (mkRegex "[^a-zA-Z0-9 .]") (T.unpack s) "" 


handlePosition :: H ()
handlePosition =
  method GET  getPositions <|>
  method POST savePosition
  where
    getPositions = do
      positions <- withDb $ \conn -> Db.listPositions conn 
      writeJSON positions
      getResponse >>= finishWith . setResponseCode 200 

    savePosition = do
      newPosition <- getJSON
      -- Uncomment the following line to show the position just received. 
      -- liftIO $ print $ show newPosition
      
      --  Note that we've not checked that inputs are valid, 
      --  but in a proper application, we ought to do some verification 
      --  that what we are receiving is sensible. 
      --
      either (const $ return ()) persist newPosition
        where
          persist p = do
            let cleanPosition = fmap filterText p  
            savedPosition <- withDb $ \conn -> Db.savePosition conn cleanPosition 
            writeJSON savedPosition  


-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [  ("/api/position",     handlePosition)
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    -- addRoutes must be called before heistInit - heist wants to
    -- serve "" itself which means our mainPage handler never gets a
    -- chance to get called.
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- Initialize auth that's backed by an sqlite database
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let conn = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar conn Db.createTables

    addAuthSplices auth
    return $ App h s d a

