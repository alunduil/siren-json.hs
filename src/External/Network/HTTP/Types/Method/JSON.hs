{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : External.Network.HTTP.Types.Method.JSON
Description : Method FromJSON and ToJSON Instances
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Method Instances for FromJSON and ToJSON.
-}
module External.Network.HTTP.Types.Method.JSON where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.ByteString.Char8 (unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.Method (StdMethod, parseMethod, renderStdMethod)

instance FromJSON StdMethod where
  parseJSON = withText "StdMethod" $ either (fail . unpack) return . parseMethod . encodeUtf8

instance ToJSON StdMethod where
  toJSON = toJSON . decodeUtf8 . renderStdMethod
