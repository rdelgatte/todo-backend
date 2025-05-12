{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model (TodoItem (..), TodoItemSpec (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (genericToJSON)
import Data.Char (toLower)
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

instance Aeson.ToJSON TodoItem where
  toJSON = genericToJSON $ aesonOptions "TodoItem"

data TodoItemSpec = TodoItemSpec
  { todoItemSpecTitle :: T.Text,
    todoItemSpecDescription :: Maybe T.Text
  }

instance Aeson.FromJSON TodoItemSpec where
  parseJSON = Aeson.withObject "TodoItemSpec" $ \obj -> do
    title <- obj .: "title"
    description <- obj .:? "description"
    pure $ TodoItemSpec title description

aesonOptions :: String -> Aeson.Options
aesonOptions prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = constructorModifier prefix
    }

constructorModifier :: String -> String -> String
constructorModifier prefix = lowerCaseFirst . drop lengthPrefix
  where
    lengthPrefix = length prefix
    lowerCaseFirst (x : xs) = toLower x : xs
    lowerCaseFirst "" = ""
