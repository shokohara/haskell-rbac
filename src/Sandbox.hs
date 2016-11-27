module Sandbox() where

import Prelude
import Data.String.Utils (strip)
import Data.List (head)

ruby :: String -> String
ruby = head . map strip . words

