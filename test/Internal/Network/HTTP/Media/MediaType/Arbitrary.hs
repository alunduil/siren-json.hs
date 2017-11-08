{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Internal.Network.HTTP.Media.MediaType.Arbitrary
Description : Arbitrary Instances for Network.HTTP.Media.MediaType
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Arbitrary instances for "Network.HTTP.Media.MediaType".
-}
module Internal.Network.HTTP.Media.MediaType.Arbitrary () where

import Prelude hiding (concat)

import Control.Monad (replicateM)
import Data.ByteString (append, concat, ByteString)
import Data.ByteString.Char8 (singleton)
import Network.HTTP.Media.MediaType ((/:), (//), MediaType)
import Test.QuickCheck (Arbitrary (arbitrary), choose, elements, Gen, listOf, oneof, sized)

--
--
-- Note: parameter---paramter values are supposed to be unrestricted but due to
--       the way that quickcheck validates values it's best if these are
--       printable.  Until we can use the new instances in quickcheck-2.10.*, we
--       shall simply use restrictedName for the values as well.
instance Arbitrary MediaType where
  arbitrary =
    do n  <- (//) <$> restrictedName <*> restrictedName
       ps <- listOf $ (,) <$> restrictedName <*> restrictedName -- see parameter note above
       return $ foldl (/:) n ps

-- * RFC 6838 Generators

restrictedName :: Gen ByteString
restrictedName = sized $ \ s ->
  do n  <- choose (0, min 126 s)
     rs <- concat <$> replicateM n restrictedNameChar
     (`append` rs) <$> restrictedNameFirst

restrictedNameFirst :: Gen ByteString
restrictedNameFirst = singleton <$> oneof [alpha, digit]

restrictedNameChar :: Gen ByteString
restrictedNameChar = singleton <$> oneof [ alpha
                                         , digit
                                         , elements ['!', '#', '$', '&', '-', '^', '_', '.', '+']
                                         ]

-- * RFC 2234 Generators

alpha :: Gen Char
alpha = elements $ ['a'..'z'] ++ ['A'..'Z']

digit :: Gen Char
digit = elements ['0'..'9']
