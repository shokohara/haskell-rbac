{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Operation where

import Data.Maybe
import qualified Data.Text as T
import Data.Tuple
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Types

data Operation = Create | Read deriving (Show, Read, Eq)
derivePersistField "Operation"

-- data OperationType = Create | Read deriving (Show, Read, Eq, Bounded)
--
-- instance Enum OperationType where
--   fromEnum = fromJust . flip lookup table
--   toEnum = fromJust . flip lookup (map swap table)
--
-- table = [(Create, 0), (Read, 1)]
--
-- safeToEnum :: (Enum t, Bounded t) => Int -> Maybe t
-- safeToEnum i = let r = toEnum i
--                    max = maxBound `asTypeOf` r
--                    min = minBound `asTypeOf` r
--                 in if i >= fromEnum min && i <= fromEnum max then Just r else Nothing
--
-- instance PersistField OperationType where
--   toPersistValue = PersistInt64 . fromIntegral . fromEnum
--   fromPersistValue (PersistInt64 i) = maybe (Left . T.pack $ ("Expected [" ++ show (minBound :: OperationType) ++ "-" ++ show (maxBound :: OperationType) ++ "] Integer, received: ")) Right ((safeToEnum . fromIntegral) i)
--   fromPersistValue i = Left . T.pack $ ("Expected [" ++ show (minBound :: OperationType) ++ "-" ++ show (maxBound :: OperationType) ++ "] Integer, received: ")
