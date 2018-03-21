{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Lens
  
import Reflex.Bulma.Elements (icon)

import Web.FontAwesomeType

type HeaderHandler t m b = (T.Text -> m ()) -> (FontAwesome -> m ()) -> m b
type FooterHandler t m b = (forall c. c -> T.Text -> m (Event t c))  -> m b

card :: forall t m a. MonadWidget t m
     => [T.Text]
     -> ((forall b. HeaderHandler t m b -> m b)
      -> (forall b.                 m b -> m b)
      -> (forall b. FooterHandler t m b -> m b) 
     -> m a) 
     -> m a
card classes f =
  div' "card" classes $ f header content footer_
  where
    header :: forall b. ((T.Text -> m ()) -> (FontAwesome -> m ()) -> m b) -> m b
    header m = divClass "card-header" $ m title icon_
      where
        title = p "card-header-title" . text
        icon_ = a "card-header-icon" . icon []

    content :: forall b. m b -> m b
    content m = divClass "card-content" $ m

    footer_ :: forall b. ((forall c. c -> T.Text -> m (Event t c)) -> m b) -> m b
    footer_ m = footer "card-footer" $ m item
     where
       item :: forall c. c -> T.Text -> m (Event t c)
       item x t = (\(e, _) -> x <$ domEvent Click e) <$> elAttr' "a" (Map.singleton "class" "card-footer-item") (text t)
