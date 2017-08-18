{-# LANGUAGE OverloadedStrings #-}

module Reflex.Bulma.Internal where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map

div' :: MonadWidget t m => T.Text -> [T.Text] -> m a -> m a
div' x = divClass . T.unwords . (x:)

span' :: MonadWidget t m => T.Text -> [T.Text] -> m a -> m a
span' x = elClass "span" . T.unwords . (x:)

p' :: MonadWidget t m => T.Text -> [T.Text] -> m a -> m a
p' x = elClass "p" . T.unwords . (x:)

figure' :: MonadWidget t m => T.Text -> [T.Text] -> m a -> m a
figure' x = elClass "figure" . T.unwords . (x:)

footer' :: MonadWidget t m => T.Text -> [T.Text] -> m a -> m a
footer' x = elClass "footer" . T.unwords . (x:)

p :: MonadWidget t m => T.Text -> m a -> m a
p x = elClass "p" x

figure :: MonadWidget t m => T.Text -> m a -> m a
figure x = elClass "figure" x

footer :: MonadWidget t m => T.Text -> m a -> m a
footer x = elClass "footer" x

a :: MonadWidget t m => T.Text -> m a -> m a
a x = elClass "a" x

