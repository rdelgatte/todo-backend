{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (Item (..), ItemSpec (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Item = Item
  { itemIdentifier :: Maybe Int64,
    itemTitle :: T.Text,
    itemDescription :: Maybe T.Text,
    itemCompleted :: Bool
  }
  deriving (Generic, Show, Eq)

instance Aeson.ToJSON Item

data ItemSpec = ItemSpec
  { itemSpecTitle :: T.Text,
    itemSpecDescription :: Maybe T.Text
  }

instance Aeson.FromJSON ItemSpec where
  parseJSON = Aeson.withObject "ItemSpec" $ \obj -> do
    title <- obj .: "title"
    description <- obj .:? "description"
    pure $ ItemSpec title description