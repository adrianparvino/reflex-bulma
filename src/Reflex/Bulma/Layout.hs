{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Reflex.Bulma.Layout
  ( HeroChildren
  , hero
  ) where

import Reflex
import Reflex.Dom

import Data.Text (Text)
import qualified Data.Text as T

type HeroChildren m a b c = ( Maybe ([Text], ((Maybe b, Maybe c) -> m a))
                            , Maybe ([Text], ((Maybe a, Maybe c) -> m b))
                            , Maybe ([Text], ((Maybe a, Maybe b) -> m c)) )

hero :: MonadWidget t m => [Text] -> HeroChildren m a b c -> m (Maybe a, Maybe b, Maybe c)
hero classes (m1, m2, m3) = elClass "section" (T.unwords $ "hero":classes) $ mdo
  x1 <- traverse (\(c1, m1) -> (divClass (T.unwords $ "hero-head":c1)) $ m1 (x2, x3)) m1
  x2 <- traverse (\(c2, m2) -> (divClass (T.unwords $ "hero-body":c2)) $ m2 (x1, x3)) m2
  x3 <- traverse (\(c3, m3) -> (divClass (T.unwords $ "hero-foot":c3)) $ m3 (x1, x2)) m3

  return (x1, x2, x3)
