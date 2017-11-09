{-|
Module      : External.Network.HTTP.Types.Method.JSONSpec
Description : Tests for External.Network.HTTP.Types.Method.JSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.HTTP.Types.Method.JSON".
-}
module External.Network.HTTP.Types.Method.JSONSpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.HTTP.Types.Method (StdMethod)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Invariant ((<=>))

import External.Network.HTTP.Types.Method.Arbitrary ()
import External.Network.HTTP.Types.Method.JSON ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "fromJust . decode . encode == id" (fromJust . decode . encode <=> id :: StdMethod -> Bool)
