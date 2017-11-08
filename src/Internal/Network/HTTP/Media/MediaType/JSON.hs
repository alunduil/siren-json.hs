{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Internal.Network.HTTP.Media.MediaType.JSON
Description : MediaType FromJSON and ToJSON Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

MediaType Instances for FromJSON and ToJSON.
-}
module Internal.Network.HTTP.Media.MediaType.JSON where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.String (fromString)
import Data.Text (pack, unpack)
import Network.HTTP.Media.MediaType (MediaType)

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ return . fromString . unpack

instance ToJSON MediaType where
  toJSON = toJSON . pack . show
