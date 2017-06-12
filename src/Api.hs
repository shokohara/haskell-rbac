{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.Int (Int32)
import qualified Data.Text as T
import Data.Text (Text, concat)
import Database.HDBC (SqlValue)
import Database.Persist.MySQL
import Database.Persist.Sql
import Database.Record.FromSql
import Database.Record.ToSql
import Database.Relational.Query
import Debug.Trace
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Prelude hiding (concat)
import Servant
import Data.Time.Clock
import qualified CreateUserRequest as C

--import Action
import Permission
import Role hiding (subjectId)
import qualified Role
import RolePermission hiding (roleId, subjectId)
import Subject
import qualified Subject
import SubjectRole hiding (subjectId)
import qualified SubjectRole

selectAccount :: Relation () Subject
selectAccount = relation $ query subject

account1 :: Relation () (Subject, Role)
account1 = relation $ do
  s <- query subject
  sr <- query subjectRole
  on $ s ! Subject.id' .=. sr ! SubjectRole.subjectId'
  r <- query role
  on $ r ! Role.id' .=. sr ! SubjectRole.roleId'
  rp <- query rolePermission
  on $ r ! Role.id' .=. rp ! RolePermission.roleId'
--  p <- query permission
--  on $ p ! Permission.id' .=. rp ! RolePermission.permissionId'
--  pa <- query permissionAction
--  on $ p ! Permission.id' .=. pa ! PermissionAction.permissionId'
--  a <- query action
--  on $ a ! Action.id' .=. pa ! PermissionAction.actionId'
  return $ (,) |$| s |*| r

--instance ProductConstructor (a -> b -> c -> (a, b, c)) where
--  productConstructor = (,,)
--
--instance (FromSql SqlValue a, FromSql SqlValue b, FromSql SqlValue c) => FromSql SqlValue (a, b, c) where
--  recordFromSql = (,,) <$> recordFromSql <*> recordFromSql <*> recordFromSql
--
--instance (ToSql SqlValue a, ToSql SqlValue b, ToSql SqlValue c) => ToSql SqlValue (a, b, c) where
--  recordToSql = createRecordToSql (\(a, b, c) -> fromRecord a ++ fromRecord b ++ fromRecord c)

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
instance FromJSON C.CreateUserRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }
instance ToJSON C.CreateUserRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase }

type API = "api" :> "authority" :> ReqBody '[JSON] AuthorizeRequest :> Post '[JSON] ()
  :<|> "api" :> "users" :> ReqBody '[JSON] C.CreateUserRequest :> Post '[JSON] ()
  :<|> "api" :> "users" :> ReqBody '[JSON] C.CreateUserRequest :> Get '[JSON] ()

--selectImageByTagNameList
--    :: Bool -- ^ match any
--    →  [Text] -- ^ list of tag name
--    →  Relation () Image.Image
--selectImageByTagNameList matchAny tagNames = relation $ do
--    img ←  query Image.image
--    imgids ←  query $ imageIdFromTagNameList matchAny tagNames
--    on $ img ! Image.id' .=. imgids
--    η img

authorizeH :: AuthorizeRequest -> ExceptT ServantErr IO ()
authorizeH a = unless (1 == subjectId a && all (\f -> (==) "*" . concat . fmap f . permissions $ a) [resource, operation]) $ throwError err403

user1 :: C.CreateUserRequest -> Relation () Subject
user1 a = relation $ do
  s <- query subject
  wheres $ s ! Subject.name' .=. value (C.name a)
  return s

--createUserH :: ConnectionPool -> C.CreateUserRequest -> ExceptT ServantErr IO ()
--createUserH pool a = liftIO $ flip runSqlPersistMPool pool $ do
--  exists <- user1 a
--  case exists of
--    Just _ -> return Nothing
--    _ -> Just <$> derivedInsertValue $ do
--      Subject.name' <-# value C.name a
--      Subject.createdAt' <-# value getCurrentTime
--      Subject.updatedAt' <-# value getCurrentTime
--      Subject.deletedAt' <-# value getCurrentTime

--createUserH :: ConnectionPool -> C.CreateUserRequest -> Handler ()
--createUserH pool a = (\now -> ()) <$> getCurrentTime
--createUserH pool a = return ()

insertUser :: C.CreateUserRequest -> UTCTime -> Insert ()
insertUser a now = derivedInsertValue $ do
    Subject.name' <-# value (C.name a)
    Subject.createdAt' <-# value now
    Subject.updatedAt' <-# value now
    Subject.deletedAt' <-# value Nothing
    return unitPlaceHolder

--server :: ConnectionPool -> Server API
--server pool = authorizeH :<|> createUserH pool
--  authorizeH where
--  authorizeH ar = liftIO $ authorize ar
--  authorize :: AuthorizeRequest -> IO ()
--  authorize ar = flip runSqlPersistMPool pool $ do
--    mSubject <- selectFirst [SubjectId ==. (subjectId ar)] []
--    return ()
--
