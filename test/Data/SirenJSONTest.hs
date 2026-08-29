{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.SirenJSONTest
Description : Tests for Data.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.SirenJSON".
-}
module Data.SirenJSONTest (tests) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Maybe (fromJust, isJust)
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

decodeSucceeds :: TestName -> Maybe a -> TestTree
decodeSucceeds name = testCase name . assertBool "decode returned Nothing" . isJust

eURI :: URI
eURI = fromJust $ parseURI "http://example.com"

mEntity :: BL.ByteString
mEntity = "{}"

mEmbeddedRepresentation :: BL.ByteString
mEmbeddedRepresentation = "{\"rel\":[]}"

mEmbeddedLink :: BL.ByteString
mEmbeddedLink = "{\"href\":\"http://example.com\",\"rel\":[]}"

mLink :: BL.ByteString
mLink = "{\"href\":\"http://example.com\",\"rel\":[]}"

mAction :: BL.ByteString
mAction = "{\"href\":\"http://example.com\",\"name\":\"name\"}"

mField :: BL.ByteString
mField = "{\"name\":\"name\"}"

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
    "properties"
    [ testGroup
        "fromJust . decode . encode == normalize"
        [ testProperty "Field" (roundtrips :: Field -> Bool)
        , testProperty "Action" (roundtrips :: Action -> Bool)
        , testProperty "Link" (roundtrips :: Link -> Bool)
        , testProperty "SubEntity" (roundtrips :: SubEntity -> Bool)
        , testProperty "Entity" (roundtrips :: Entity -> Bool)
        ]
    ]

roundtrips :: (Eq a, FromJSON a, Norm a, ToJSON a) => a -> Bool
roundtrips = fromJust . decode . encode <=> normalize

subEntityTests :: TestTree
subEntityTests =
  testGroup
    "differentiate SubEntity values"
    [ testCase "SubEntity_EmbeddedRepresentation" $
        (decode mEmbeddedRepresentation :: Maybe SubEntity) @?= Just (EmbeddedRepresentation (Entity [] Map.empty [] [] [] Nothing) [])
    , testCase "SubEntity_EmbeddedLink" $
        (decode mEmbeddedLink :: Maybe SubEntity) @?= Just (EmbeddedLink (Link [] [] eURI Nothing Nothing))
    ]

hrefTests :: TestTree
hrefTests =
  testGroup
    "href"
    [ testGroup
        "decode absolute URIs only"
        [ testCase "Link" $ (decode mLink_RelativeHref :: Maybe Link) @?= Nothing
        , testCase "Action" $ (decode mAction_RelativeHref :: Maybe Action) @?= Nothing
        ]
    ]
 where
  mLink_RelativeHref = "{\"href\":\"/orders/42\",\"rel\":[]}" :: BL.ByteString
  mAction_RelativeHref = "{\"href\":\"/orders/42\",\"name\":\"name\"}" :: BL.ByteString

missingKeysTests :: TestTree
missingKeysTests =
  testGroup
    "JSON Missing Keys"
    [ testGroup
        "decode minimal JSON strings"
        [ decodeSucceeds "Field" (decode mField :: Maybe Field)
        , decodeSucceeds "Action" (decode mAction :: Maybe Action)
        , decodeSucceeds "Link" (decode mLink :: Maybe Link)
        , decodeSucceeds "SubEntity_EmbeddedRepresentation" (decode mEmbeddedRepresentation :: Maybe SubEntity)
        , decodeSucceeds "SubEntity_EmbeddedLink" (decode mEmbeddedLink :: Maybe SubEntity)
        , decodeSucceeds "Entity" (decode mEntity :: Maybe Entity)
        ]
    , testGroup
        "encode minimal data to JSON"
        [ testCase "Field" $
            encode (Field "name" [] Nothing Nothing Nothing) @?= mField
        , testCase "Action" $
            encode (Action "name" [] Nothing eURI Nothing Nothing []) @?= mAction
        , testCase "Link" $
            encode (Link [] [] eURI Nothing Nothing) @?= mLink
        , testCase "SubEntity_EmbeddedRepresentation" $
            encode (EmbeddedRepresentation (Entity [] Map.empty [] [] [] Nothing) []) @?= mEmbeddedRepresentation
        , testCase "SubEntity_EmbeddedLink" $
            encode (EmbeddedLink (Link [] [] eURI Nothing Nothing)) @?= mEmbeddedLink
        , testCase "Entity" $
            encode (Entity [] Map.empty [] [] [] Nothing) @?= mEntity
        ]
    ]
