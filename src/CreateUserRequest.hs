{-# LANGUAGE DeriveGeneric #-}
module CreateUserRequest where

import GHC.Generics
import Data.Text (Text, concat)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types

newtype CreateUserRequest = CreateUserRequest { name :: Text } deriving (Show, Generic)


