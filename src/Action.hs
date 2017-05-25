{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Action where

import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Relational
import GHC.Generics
import Model hiding (Action)
import qualified Model
import Operation
import Resource

defineTableFromPersistent ''Model.Action db
