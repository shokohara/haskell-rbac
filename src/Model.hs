{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import Data.Aeson
import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Quasi
import Database.Persist.Relational (mkHrrInstances)
import Database.Persist.TH
import Operation
import Resource

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkSave "db", mkHrrInstances]
  $(persistFileWith lowerCaseSettings "config/models")
