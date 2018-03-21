{-# LANGUAGE GADTs, RankNTypes, DataKinds, OverloadedStrings, ScopedTypeVariables #-}
module Reflex.Bulma.Grid
  ( columns
  ) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Widget

import Control.Monad.Fix

import Data.Text (Text)
import qualified Data.Text as T

columns :: forall t m a. MonadWidget t m => [Text] -> ((forall b. [Text] -> m b -> m b) -> m a) -> m a
columns classes m = divClass (T.unwords $ "columns":classes) $ m column
  where
    column :: [Text] -> m b -> m b
    column classes inner = divClass (T.unwords $ "column":classes) inner
