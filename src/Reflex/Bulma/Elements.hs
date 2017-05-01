{-# LANGUAGE OverloadedStrings #-}
module Reflex.Bulma.Elements where

import Reflex
import Reflex.Dom
import Reflex.Dom.Widget
import Reflex.Dom.Contrib.Widgets.ButtonGroup

import Data.Monoid
import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens

import Web.FontAwesomeType

div' :: MonadWidget t m => Text -> [Text] -> m a -> m a
div' x = divClass . T.unwords . (x:)

box :: MonadWidget t m => [Text] -> m a -> m a
box = div' "box"

p :: MonadWidget t m => [Text] -> m a -> m a
p = elClass "p" . T.unwords

h :: MonadWidget t m => Int -> [Text] -> m a -> m a
h level = elClass ("h" <> (T.pack . show $ level)) . T.unwords

h1 :: MonadWidget t m => [Text] -> m a -> m a
h1 = h 1

h2 :: MonadWidget t m => [Text] -> m a -> m a
h2 = h 2

h3 :: MonadWidget t m => [Text] -> m a -> m a
h3 = h 3

h4 :: MonadWidget t m => [Text] -> m a -> m a
h4 = h 4

h5 :: MonadWidget t m => [Text] -> m a -> m a
h5 = h 5

h6 :: MonadWidget t m => [Text] -> m a -> m a
h6 = h 6

blockquote :: MonadWidget t m => [Text] -> m a -> m a
blockquote = elClass "blockquote" . T.unwords

em :: MonadWidget t m => [Text] -> m a -> m a
em = elClass "em" . T.unwords

delete :: MonadWidget t m => [Text] -> m (Event t ())
delete classes = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: T.unwords ("delete":classes)) $ return ()
  return $ domEvent Click e

field :: MonadWidget t m => [Text] -> m a -> m a
field = div' "field"

container :: MonadWidget t m => [Text] -> m a -> m a
container = elClass "container" . T.unwords

label :: MonadWidget t m => [Text] -> m a -> m a
label = elClass "label" . T.unwords

textInput :: MonadWidget t m => [Text] -> Text -> m (TextInput t)
textInput classes placeholder = Reflex.Dom.textInput (def & textInputConfig_attributes .~
                                                      (return $ "placeholder" =: placeholder
                                                             <> "class"       =: T.unwords ("input":classes)))

textArea :: MonadWidget t m => [Text] -> Text -> m (TextArea t)
textArea classes placeholder = Reflex.Dom.textArea (def & textAreaConfig_attributes .~
                                                    (return $ "placeholder" =: placeholder
                                                           <> "class"       =: T.unwords ("textarea":classes)))


selectDyn :: (Ord a, MonadWidget t m) => [Text] -> a -> Dynamic t (Map a Text) -> m (Dropdown t a)
selectDyn classes i options = dropdown i options (def & dropdownConfig_attributes .~
                                                   (return $ "class" =: T.unwords ("select":classes)))

select :: (Ord a, MonadWidget t m) => [Text] -> a -> Map a Text -> m (Dropdown t a)
select classes i = selectDyn classes i . return

checkbox :: MonadWidget t m => [Text] -> Bool -> m (Checkbox t)
checkbox classes i = Reflex.Dom.Widget.checkbox i (def & checkboxConfig_attributes .~
                                                    (return $ "class" =: T.unwords ("checkbox":classes)))

-- TODO: radios
button :: MonadWidget t m => [Text] -> Text -> m (Event t ())
button classes s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: T.unwords ("button":classes)) $ text s
  return $ domEvent Click e

icon :: (MonadWidget t m) => [Text] -> FontAwesome -> m ()
icon classes i = elClass "span" (T.unwords $ "icon":classes) $ elClass "i" ("fa " <> fontAwesomeClass i) $ return ()

image :: (MonadWidget t m) => [Text] -> m () -> m ()
image classes = elClass "figure" (T.unwords $ "image":classes)

-- TODO: Progress bar

tag :: (MonadWidget t m) => [Text] -> Text -> Bool -> m (Event t ())
tag classes content withDelete = elClass "figure" (T.unwords $ "image":classes) $ text content >> if withDelete
                                                                                                  then delete []
                                                                                                  else return never
