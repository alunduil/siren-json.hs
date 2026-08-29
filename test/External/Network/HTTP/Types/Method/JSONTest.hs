{- |
Module      : External.Network.HTTP.Types.Method.JSONTest
Description : Tests for External.Network.HTTP.Types.Method.JSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.HTTP.Types.Method.JSON".
-}
module External.Network.HTTP.Types.Method.JSONTest (tests) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.HTTP.Types.Method (StdMethod)
import Network.HTTP.Types.Method.Arbitrary ()
import Test.Invariant ((<=>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import External.Network.HTTP.Types.Method.JSON ()

tests :: TestTree
tests =
  testGroup
    "External.Network.HTTP.Types.Method.JSON"
    [ testProperty "fromJust . decode . encode == id" (fromJust . decode . encode <=> id :: StdMethod -> Bool)
    ]
