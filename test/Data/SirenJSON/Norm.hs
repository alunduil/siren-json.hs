{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Data.SirenJSON.Norm
Description : Norm Instances for Data.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Norm instances for "Data.SirenJSON".

Also includes a definition for the 'Norm' class until it is proven generally
useful.
-}
module Data.SirenJSON.Norm
  ( Norm (..)
  ) where

import Data.Functor ((<$>))
import Data.Maybe (isNothing)
import Network.HTTP.Types.Method (StdMethod (GET))

import Data.SirenJSON

class Norm a where
  normalize :: a -> a
  normalize = id

instance Norm Entity where
  normalize Entity{..} = Entity eClass
                                eProperties
                                (normalize <$> eEntities)
                                (normalize <$> eLinks)
                                (normalize <$> eActions)
                                eTitle

instance Norm SubEntity where
  normalize (EmbeddedLink l) = EmbeddedLink $ normalize l
  normalize EmbeddedRepresentation{..} = EmbeddedRepresentation (normalize sEntity) sRel

instance Norm Link

instance Norm Action where
  normalize Action{..} = Action aName
                                aClass
                                (if isNothing aMethod then Just GET else aMethod)
                                aHref
                                aTitle
                                aType
                                (normalize <$> aFields)

instance Norm Field where
  normalize Field{..} = Field fName
                              fClass
                              (if isNothing fType then Just Text else fType)
                              fValue
                              fTitle
