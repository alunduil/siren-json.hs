{- |
Module      : External.Network.HTTP.Media.MediaType.JSONTest
Description : Tests for External.Network.HTTP.Media.MediaType.JSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "External.Network.HTTP.Media.MediaType.JSON".
-}
module External.Network.HTTP.Media.MediaType.JSONTest (tests) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Media.MediaType.Arbitrary ()
import Test.Invariant ((<=>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import External.Network.HTTP.Media.MediaType.JSON ()

tests :: TestTree
tests =
  testGroup
    "External.Network.HTTP.Media.MediaType.JSON"
    [ testGroup
        "properties"
        [ testProperty "fromJust . decode . encode == id" (fromJust . decode . encode <=> id :: MediaType -> Bool)
        ]
    ]
