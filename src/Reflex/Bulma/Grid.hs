{-# LANGUAGE GADTs, RankNTypes, DataKinds, OverloadedStrings #-}
module Reflex.Bulma.Grid
  ( columns
  ) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Widget

import Control.Monad.Fix

import Data.Text (Text)
import qualified Data.Text as T

columns :: MonadWidget t m => [Text] -> (forall n. MonadFix n => (forall b. [Text] -> m b -> n b) -> n a) -> m a
columns classes m = divClass (T.unwords $ "columns":classes) $ m column
  where
    column :: MonadWidget t m => [Text] -> m b -> m b
    column classes inner = divClass (T.unwords $ "column":classes) inner
