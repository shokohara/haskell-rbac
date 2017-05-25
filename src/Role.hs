{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Role where

import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Relational
import GHC.Generics
import Model hiding (Role)
import qualified Model

defineTableFromPersistent ''Model.Role db
