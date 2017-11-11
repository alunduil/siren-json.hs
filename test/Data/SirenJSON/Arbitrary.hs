{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Data.SirenJSON.Arbitrary
Description : Arbitrary Instances for Data.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Arbitrary instances for "Data.SirenJSON".
-}
module Data.SirenJSON.Arbitrary where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), elements, oneof, scale)
import Test.QuickCheck.Instances ()

import Data.SirenJSON
import External.Network.HTTP.Media.MediaType.Arbitrary ()
import External.Network.HTTP.Types.Method.Arbitrary ()
import External.Network.URI.Arbitrary ()

instance Arbitrary Entity where
  arbitrary = Entity <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

  shrink Entity{..} = mapMaybe e eEntities ++
                      [ Entity eClass' eProperties' eEntities' eLinks' eActions' eTitle' | (eClass', eProperties', eEntities', eLinks', eActions', eTitle') <- shrink (eClass, eProperties, eEntities, eLinks, eActions, eTitle) ]
    where e EmbeddedRepresentation{..} = Just sEntity
          e _                          = Nothing

instance Arbitrary SubEntity where
  arbitrary = oneof [ EmbeddedLink <$> arbitrary
                    , EmbeddedRepresentation <$> scale (`div` 2) arbitrary
                                             <*> arbitrary
                    ]

  shrink (EmbeddedLink l)           = [ EmbeddedLink l' | l' <- shrink l ]
  shrink EmbeddedRepresentation{..} = map EmbeddedLink (eLinks sEntity) ++
                                      filter isEntity (eEntities sEntity) ++
                                      [ EmbeddedRepresentation sEntity' sRel' | (sEntity', sRel') <- shrink (sEntity, sRel) ]
    where isEntity (EmbeddedRepresentation _ _) = True
          isEntity _                            = False

instance Arbitrary Link where
  arbitrary = Link <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

  shrink Link{..} = [ Link lClass' lRel' lHref' lType' lTitle' | (lClass', lRel', lHref', lType', lTitle') <- shrink (lClass, lRel, lHref, lType, lTitle) ]

instance Arbitrary Action where
  arbitrary = Action <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

  shrink Action{..} = [ Action aName' aClass' aMethod' aHref' aTitle' aType' aFields' | (aName', aClass', aMethod', aHref', aTitle', aType', aFields') <- shrink (aName, aClass, aMethod, aHref, aTitle, aType, aFields) ]

instance Arbitrary Field where
  arbitrary = Field <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

  shrink Field{..} = [ Field fName' fClass' fType' fValue' fTitle' | (fName', fClass', fType', fValue', fTitle') <- shrink (fName, fClass, fType, fValue, fTitle) ]

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
