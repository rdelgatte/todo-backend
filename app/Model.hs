{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model (TodoItem (..), TodoItemSpec (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import GHC.Generics (Generic)

data TodoItem = TodoItem
  { todoItemIdentifier :: Maybe Int64,
    todoItemTitle :: T.Text,
    todoItemDescription :: Maybe T.Text,
    todoItemCompleted :: Bool
  }
  deriving (Generic, Show, Eq)

instance Aeson.ToJSON TodoItem

data TodoItemSpec = TodoItemSpec
  { todoItemSpecTitle :: T.Text,
    todoItemSpecDescription :: Maybe T.Text
  }

instance Aeson.FromJSON TodoItemSpec where
  parseJSON = Aeson.withObject "TodoItemSpec" $ \obj -> do
    title <- obj .: "title"
    description <- obj .:? "description"
    pure $ TodoItemSpec title description