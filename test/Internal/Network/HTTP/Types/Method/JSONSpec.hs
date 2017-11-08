{-|
Module      : Internal.Network.HTTP.Types.Method.JSONSpec
Description : Tests for Internal.Network.HTTP.Types.Method.JSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Internal.Network.HTTP.Types.Method.JSON".
-}
module Internal.Network.HTTP.Types.Method.JSONSpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.HTTP.Types.Method (StdMethod)
import Test.Hspec (describe, hspec, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Invariant ((<=>))

import Internal.Network.HTTP.Types.Method.Arbitrary ()
import Internal.Network.HTTP.Types.Method.JSON ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "fromJust . decode . encode == id" (fromJust . decode . encode <=> id :: StdMethod -> Bool)
