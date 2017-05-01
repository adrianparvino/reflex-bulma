{-# LANGUAGE RankNTypes, DataKinds, OverloadedStrings #-}
module Reflex.Bulma.Grid
  ( columns
  ) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Widget

import Data.Monoid

import Control.Monad.Fix

import Data.Functor.Const

import Data.Text (Text)
import qualified Data.Text as T

columns :: MonadWidget t m => [Text] -> (forall p. ([Text] -> m a -> m (Const a p)) -> m (Const a p)) -> m a
columns classes m = divClass (T.unwords $ "columns":classes) . fmap getConst $ m column
  where
    column :: MonadWidget t m => forall p. [Text] -> m a -> m (Const a p)
    column classes inner = Const <$> divClass (T.unwords $ "column":classes) inner
