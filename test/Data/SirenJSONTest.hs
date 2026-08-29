{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Data.SirenJSONTest
Description : Tests for Data.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.SirenJSON".
-}
module Data.SirenJSONTest (tests) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Maybe (fromJust, isJust, isNothing)
import Network.URI (URI, parseURI)
import Test.Invariant ((<=>))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestName, TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckMaxSize (QuickCheckMaxSize), testProperty)

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Map.Strict as Map (empty)

import Data.SirenJSON
import Data.SirenJSON.Arbitrary ()
import Data.SirenJSON.Norm (Norm (normalize))

-- | A test name, a value with no optional key set, and the JSON it encodes to.
data Minimal a = Minimal TestName a BL.ByteString

exampleURI :: URI
exampleURI = fromJust $ parseURI "http://example.com"

minimalField :: Minimal Field
minimalField =
  Minimal
    "Field"
    (Field "name" [] Nothing Nothing Nothing)
    "{\"name\":\"name\"}"

minimalAction :: Minimal Action
minimalAction =
  Minimal
    "Action"
    (Action "name" [] Nothing exampleURI Nothing Nothing [])
    "{\"href\":\"http://example.com\",\"name\":\"name\"}"

minimalLink :: Minimal Link
minimalLink =
  Minimal
    "Link"
    (Link [] [] exampleURI Nothing Nothing)
    "{\"href\":\"http://example.com\",\"rel\":[]}"

minimalEntity :: Minimal Entity
minimalEntity =
  Minimal
    "Entity"
    (Entity [] Map.empty [] [] [] Nothing)
    "{}"

minimalEmbeddedLink :: Minimal SubEntity
minimalEmbeddedLink = Minimal "SubEntity_EmbeddedLink" (EmbeddedLink link) json
 where
  Minimal _ link json = minimalLink

minimalEmbeddedRepresentation :: Minimal SubEntity
minimalEmbeddedRepresentation =
  Minimal
    "SubEntity_EmbeddedRepresentation"
    (EmbeddedRepresentation entity [])
    "{\"rel\":[]}"
 where
  Minimal _ entity _ = minimalEntity

decodeSucceeds :: TestName -> Maybe a -> TestTree
decodeSucceeds name = testCase name . assertBool "decode returned Nothing" . isJust

decodeFails :: TestName -> Maybe a -> TestTree
decodeFails name = testCase name . assertBool "decode returned a value" . isNothing

decodesTo :: (Eq a, FromJSON a, Show a) => Minimal a -> TestTree
decodesTo (Minimal name value json) = testCase name $ decode json @?= Just value

{- | Rows differ in type, so no list can hold them for a second pass. Each row
  yields both of its cases at once, for 'unzip' to sort into their groups.
-}
minimalCases :: forall a. (FromJSON a, ToJSON a) => Minimal a -> (TestTree, TestTree)
minimalCases (Minimal name value json) =
  ( decodeSucceeds name (decode json :: Maybe a)
  , testCase name $ encode value @?= json
  )

roundtrips :: (Eq a, FromJSON a, Norm a, ToJSON a) => a -> Bool
roundtrips = fromJust . decode . encode <=> normalize

tests :: TestTree
tests =
  localOption (QuickCheckMaxSize 25) $
    testGroup
      "application/vnd.siren+json"
      [ propertiesTests
      , subEntityTests
      , hrefTests
      , missingKeysTests
      ]

propertiesTests :: TestTree
propertiesTests =
  testGroup
    "fromJust . decode . encode == normalize"
    [ testProperty "Field" (roundtrips :: Field -> Bool)
    , testProperty "Action" (roundtrips :: Action -> Bool)
    , testProperty "Link" (roundtrips :: Link -> Bool)
    , testProperty "SubEntity" (roundtrips :: SubEntity -> Bool)
    , testProperty "Entity" (roundtrips :: Entity -> Bool)
    ]

subEntityTests :: TestTree
subEntityTests =
  testGroup
    "differentiate SubEntity values"
    [ decodesTo minimalEmbeddedRepresentation
    , decodesTo minimalEmbeddedLink
    ]

hrefTests :: TestTree
hrefTests =
  testGroup
    "href decodes absolute URIs only"
    [ decodeFails "Link" (decode relativeLink :: Maybe Link)
    , decodeFails "Action" (decode relativeAction :: Maybe Action)
    ]
 where
  relativeLink = "{\"href\":\"/orders/42\",\"rel\":[]}" :: BL.ByteString
  relativeAction = "{\"href\":\"/orders/42\",\"name\":\"name\"}" :: BL.ByteString

missingKeysTests :: TestTree
missingKeysTests =
  testGroup
    "JSON Missing Keys"
    [ testGroup "decode minimal JSON strings" decodeTests
    , testGroup "encode minimal data to JSON" encodeTests
    ]
 where
  (decodeTests, encodeTests) =
    unzip
      [ minimalCases minimalField
      , minimalCases minimalAction
      , minimalCases minimalLink
      , minimalCases minimalEmbeddedRepresentation
      , minimalCases minimalEmbeddedLink
      , minimalCases minimalEntity
      ]
