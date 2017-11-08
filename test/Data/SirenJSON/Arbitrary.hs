{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Data.SirenJSON.Arbitrary
Description : Arbitrary Instances for Data.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Arbitrary instances for "Data.SirenJSON".
-}
module Data.SirenJSON.Arbitrary where

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck (Arbitrary (arbitrary), elements, oneof, scale)
import Test.QuickCheck.Instances ()

import Data.SirenJSON
import Internal.Network.HTTP.Media.MediaType.Arbitrary ()
import Internal.Network.HTTP.Types.Method.Arbitrary ()
import Internal.Network.URI.Arbitrary ()

instance Arbitrary Entity where
  arbitrary = Entity <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary SubEntity where
  arbitrary = oneof [ EmbeddedLink <$> arbitrary
                    , EmbeddedRepresentation <$> scale (`div` 2) arbitrary
                                             <*> arbitrary
                    ]

instance Arbitrary Link where
  arbitrary = Link <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary Action where
  arbitrary = Action <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary Field where
  arbitrary = Field <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary InputType where
  arbitrary = elements [ Hidden
                       , Text
                       , Search
                       , Tel
                       , URL
                       , Email
                       , Password
                       , DateTime
                       , Date
                       , Month
                       , Week
                       , Time
                       , DateTimeLocal
                       , Number
                       , Range
                       , Color
                       , CheckBox
                       , Radio
                       , File
                       ]
