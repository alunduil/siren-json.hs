{-|
Module      : External.Network.HTTP.Media.MediaType.ArbitrarySpec
Description : Tests for External.Network.HTTP.Media.MediaType.Arbitrary
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.HTTP.Media.MediaType.Arbitrary".
-}
module External.Network.HTTP.Media.MediaType.ArbitrarySpec (main, spec) where

import Prelude hiding (null)

import Data.ByteString (null)
import Data.CaseInsensitive (original)
import Network.HTTP.Media.MediaType (mainType, subType)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)

import External.Network.HTTP.Media.MediaType.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    do prop "not . null . mainType" $ not . null . original . mainType
       prop "not . null . subType"  $ not . null . original . subType
