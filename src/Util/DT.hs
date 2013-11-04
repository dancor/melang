module Util.DT where

import Control.Arrow
import qualified Data.Text as DT

breakOnCh :: Char -> DT.Text -> (DT.Text, DT.Text)
breakOnCh c = second DT.tail . DT.break (== c)
