module Idris.Util (safeHead) where

safeHead _ (y:_) = y
safeHead x []    = x
