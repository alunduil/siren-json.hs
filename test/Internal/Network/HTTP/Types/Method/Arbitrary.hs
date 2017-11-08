{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Internal.Network.HTTP.Types.Method.Arbitrary
Description : Arbitrary Instances for Network.HTTP.Types.Method
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Arbitrary instances for "Network.HTTP.Types.Method".
-}
module Internal.Network.HTTP.Types.Method.Arbitrary () where

import Network.HTTP.Types.Method (StdMethod (..))
import Test.QuickCheck (Arbitrary (arbitrary), elements)

instance Arbitrary StdMethod where
  arbitrary = elements [ GET
                       , POST
                       , HEAD
                       , PUT
                       , DELETE
                       , TRACE
                       , CONNECT
                       , OPTIONS
                       , PATCH
                       ]
