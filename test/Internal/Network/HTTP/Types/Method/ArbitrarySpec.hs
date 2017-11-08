{-|
Module      : Internal.Network.HTTP.Types.Method.ArbitrarySpec
Description : Tests for Internal.Network.HTTP.Types.Method.Arbitrary
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Internal.Network.HTTP.Types.Method.Arbitrary".
-}
module Internal.Network.HTTP.Types.Method.ArbitrarySpec (main, spec) where

import Data.Either (isRight)
import Network.HTTP.Types.Method (parseMethod, renderStdMethod)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec (describe, hspec, Spec)

import Internal.Network.HTTP.Types.Method.Arbitrary ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "properties" $
    prop "isRight . parseMethod . renderStdMethod" $ isRight . parseMethod . renderStdMethod
