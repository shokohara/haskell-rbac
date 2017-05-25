{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PermissionAction where

import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Relational
import GHC.Generics
import Model hiding (PermissionAction)
import qualified Model

defineTableFromPersistent ''Model.PermissionAction db
