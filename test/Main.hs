-- SPDX-FileCopyrightText: 2017 Alex Brandt <alunduil@alunduil.com>
-- SPDX-License-Identifier: MIT

module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Data.SirenJSONTest (tests)
import qualified External.Network.HTTP.Media.MediaType.JSONTest (tests)
import qualified External.Network.HTTP.Types.Method.JSONTest (tests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "siren-json"
      [ Data.SirenJSONTest.tests
      , External.Network.HTTP.Media.MediaType.JSONTest.tests
      , External.Network.HTTP.Types.Method.JSONTest.tests
      ]
