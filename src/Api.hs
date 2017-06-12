{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api where

import Prelude hiding (concat)
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Casing
import qualified Data.Text as T
import Control.Monad.Trans.Except
import Control.Monad.Except
import Network.Wai
import Database.Persist.MySQL
import Database.Persist.Sql
import Network.Wai.Handler.Warp
import Servant
import Data.Text (Text, concat)
import Models
import GHC.Generics
import Debug.Trace
import Control.Monad

data PermissionRequest = PermissionRequest { resource :: Text, operation :: Text } deriving (Show, Generic)
data AuthorizeRequest = AuthorizeRequest { subjectId :: Integer, permissions :: [PermissionRequest] } deriving (Show, Generic)

instance FromJSON PermissionRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }
instance FromJSON AuthorizeRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }
instance ToJSON PermissionRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase }
instance ToJSON AuthorizeRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase }

type API = "api" :> "authority" :> ReqBody '[JSON] AuthorizeRequest :> Post '[JSON] ()

authorizeH :: AuthorizeRequest -> ExceptT ServantErr IO ()
authorizeH a = unless (1 == subjectId a && all (\f -> (==) "*" . concat . fmap f . permissions $ a) [resource, operation]) $ throwError err403

server :: Server API
server = authorizeH
