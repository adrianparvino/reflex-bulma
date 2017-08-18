{-# LANGUAGE OverloadedStrings #-}

module Reflex.Bulma.Components where

import Reflex.Bulma.Internal
import qualified Text as T
import Data.Monoid

card :: MonadWidget t m => Maybe Text -> Maybe Text -> Maybe Text -> m b -> [(Text, (Maybe T.Text), a)] -> m (Event t a)
card title icon image content footers =
  divClass "card" $ do
    when (title >> icon /= mempty) $
      divClass "card-image" $ do
        for title $ \t ->
          p' "card-header-title" t
        for icon $ \i ->
          undefined
    divClass "card-content" $ do
      content
    when (footer /= []) $
      leftmost <$> footer "card-footer" $ do
        for footer $ \(l, url, r) ->
          a url r
          
