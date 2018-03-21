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

card :: forall t m a. MonadWidget t m
     => [T.Text]
     -> ((forall b. ((forall c. m c -> m c) -> (forall c. m c -> m c) -> m b) -> m b)
      -> (forall b. m b                                                       -> m b)
      -> (forall b. ((forall c. m c -> m c) -> m b)                           -> m b)
      -> m a)
     -> m a
card classes f =
  div' "card" classes $ f header content footer_
  where
    header :: forall b. ((forall c. m c -> m c) -> (forall c. m c -> m c) -> m b) -> m b
    header m = divClass "card-header" $ m title icon
      where
        title :: forall c. m c -> m c
        title = p "card-header-title"
        icon :: forall c. m c -> m c
        icon  = a "card-header-icon"

    content :: forall b. m b -> m b
    content m = divClass "card-content" $ m

    footer_ :: forall b. ((forall c. m c -> m c) -> m b) -> m b
    footer_ m = footer "card-footer" $ m item
     where
       item :: forall c. m c -> m c
       item = a "card-footer-item"
