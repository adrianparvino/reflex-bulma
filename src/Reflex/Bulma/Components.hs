{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Reflex.Bulma.Components where

import Reflex.Dom
import Reflex.Bulma.Internal
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe
import Data.Traversable
import Control.Monad
  
import Reflex.Bulma.Elements (icon)

import Web.FontAwesomeType

card :: MonadWidget t m => [T.Text] -> Maybe (T.Text, Maybe FontAwesome) -> m (Dynamic t a) -> [(Maybe T.Text, T.Text, b)] -> m (Event t (a, b))
card classes header content footers =
  div' "card" classes $ do
    for header $ \(t, i) -> do
      divClass "card-header" $ do
        p "card-header-title" . text $ t
      for i $ a "card-header-icon" . icon []

    x <- divClass "card-content" $ do
      content

    case footers of
      [] -> return never
      xs -> footer "card-footer" . fmap (attachPromptlyDyn x . leftmost) .
        for xs $ \(url, content, r) -> do
          (e, _) <- elAttr' "a" (Map.singleton "class" "card-footer-item" <>
                                 fromMaybe (("href",) <$> url)) . text $ content
          return $ r <$ domEvent Click e
          where
            fromMaybe Nothing = mempty
            fromMaybe (Just (k, v)) = Map.singleton k v
