{-# LANGUAGE OverloadedStrings #-}

module App where

import Api
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Aeson hiding (decode)
import Data.Maybe
import Data.Foldable
import Database.Persist.MySQL
import Models
import Servant
import System.Environment
import Control.Exception
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app p = serve api server

makeApp :: Text -> IO Application
makeApp s = do
  host <- fromMaybe "127.0.0.1" <$> lookupEnv "MYSQL_HOST"
  port <- maybe 3306 read <$> lookupEnv "MYSQL_PORT"
  poolSize <- maybe 10 read <$> lookupEnv "MYSQL_POOLSIZE" -- Here may throw exception from read function. This is intentional decision in software design.
  database <- fromMaybe "ope" <$> lookupEnv "MYSQL_DATABASE"
  user <- fromMaybe "root" <$> lookupEnv "MYSQL_USER"
  password <- fromMaybe "root" <$> lookupEnv "MYSQL_PASSWORD"
  pool <- runStdoutLoggingT $ createMySQLPool defaultConnectInfo { connectHost = host, connectPort = port, connectDatabase = database, connectUser = user, connectPassword = password } poolSize
  runSqlPool (runMigrationUnsafe migrateAll) pool
  return $ app pool

makeTestApp :: Text -> IO Application
makeTestApp = makeApp

run :: Text -> IO ()
run s = withStdoutLogger $ \aplogger -> do
  let settings = setPort 3000 $ setLogger aplogger defaultSettings
  runSettings settings =<< makeApp s
