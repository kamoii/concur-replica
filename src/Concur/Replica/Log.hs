{-# LANGUAGE OverloadedStrings #-}
module Concur.Replica.Log
  ( Log(..)
  , format
  ) where

import qualified Colog            as Co
import qualified Chronos          as Ch
import qualified Replica.Run.Log  as R
import qualified Data.Text        as T

data Log
  = Greeting T.Text
  | ReplicaLog R.Log
  | WaiLog T.Text

format :: (Ch.Time, Log) -> T.Text
format (t, log) =
  case log of
    Greeting txt -> R.encodeTime t <> "\n" <> txt
    ReplicaLog l -> R.format (t, l)
    WaiLog l     -> R.encodeTime t <> " " <> l
