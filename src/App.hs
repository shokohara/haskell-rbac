{-# LANGUAGE OverloadedStrings #-}

module App where

import Api
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson hiding (decode)
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Database.Persist.MySQL
import Model
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.Environment
import Servant.Mock
--import CreateUserRequest

api :: Proxy API
api = Proxy

--app :: ConnectionPool -> Application
--app p = serve api $ server p

--makeApp :: Text -> IO Application
--makeApp s = do
--  host <- fromMaybe "127.0.0.1" <$> lookupEnv "MYSQL_HOST"
--  poolSize <- maybe 10 read <$> lookupEnv "MYSQL_POOLSIZE" -- Here may throw exception from read function. This is intentional decision in software design.
--  database <- fromMaybe "rbac" <$> lookupEnv "MYSQL_DATABASE"
--  user <- fromMaybe "root" <$> lookupEnv "MYSQL_USER"
--  password <- fromMaybe "" <$> lookupEnv "MYSQL_PASSWORD"
--  pool <- runStdoutLoggingT $ createMySQLPool defaultConnectInfo { connectHost = host, connectDatabase = database, connectUser = user, connectPassword = password } poolSize
--  runSqlPool (runMigrationUnsafe migrateAll) pool
--  return $ app pool
makeApp :: Text -> IO Application
makeApp s = return $ mock api

makeTestApp :: Text -> IO Application
makeTestApp = makeApp

run :: Text -> IO ()
run s = withStdoutLogger $ \aplogger -> do
  let settings = setPort 3000 $ setLogger aplogger defaultSettings
  runSettings settings =<< makeApp s
