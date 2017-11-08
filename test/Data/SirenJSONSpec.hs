{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.SirenJSONSpec
Description : Tests for Data.SirenJSON
Copyright   : (c) Alex Brandt, 2017
License     : MIT

Tests for "Data.SirenJSON".
-}
module Data.SirenJSONSpec (main, spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (fromJust, isJust)
import Network.URI (parseURIReference)
import Test.Hspec (context, describe, hspec, it, shouldBe, Spec)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.Invariant ((<=>))
import Test.QuickCheck.Instances ()

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Map.Strict as Map (empty)

import Data.SirenJSON
import Data.SirenJSON.Arbitrary ()
import Data.SirenJSON.Norm (Norm (normalize))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "application/vnd.siren+json" $ modifyMaxSize (const 25) $
    do describe "properties" $
         context "fromJust . decode . encode == normalize" $
           do prop "Field"     (fromJust . decode . encode <=> normalize :: Field -> Bool)
              prop "Action"    (fromJust . decode . encode <=> normalize :: Action -> Bool)
              prop "Link"      (fromJust . decode . encode <=> normalize :: Link -> Bool)
              prop "SubEntity" (fromJust . decode . encode <=> normalize :: SubEntity -> Bool)
              prop "Entity"    (fromJust . decode . encode <=> normalize :: Entity -> Bool)

       describe "differentiate SubEntity values" $
         do it "SubEntity_EmbeddedRepresentation" $
              (decode mSubEntity_EmbeddedRepresentation :: Maybe SubEntity) `shouldBe` Just (EmbeddedRepresentation (Entity [] Map.empty [] [] [] Nothing) [])

            it "SubEntity_EmbeddedLink" $
              (decode mSubEntity_EmbeddedLink :: Maybe SubEntity) `shouldBe` Just (EmbeddedLink (Link [] [] eURI Nothing Nothing))

       describe "JSON Missing Keys" $
         do context "decode minimal JSON strings" $
              do it "Field"                            $ isJust (decode mField :: Maybe Field)
                 it "Action"                           $ isJust (decode mAction :: Maybe Action)
                 it "Link"                             $ isJust (decode mLink :: Maybe Link)
                 it "SubEntity_EmbeddedRepresentation" $ isJust (decode mSubEntity_EmbeddedRepresentation :: Maybe SubEntity)
                 it "SubEntity_EmbeddedLink"           $ isJust (decode mSubEntity_EmbeddedLink :: Maybe SubEntity)
                 it "Entity"                           $ isJust (decode mEntity :: Maybe Entity)

            context "encode minimal data to JSON" $
              do it "Field" $
                   encode (Field "name" [] Nothing Nothing Nothing) `shouldBe` mField

                 it "Action" $
                   encode (Action "name" [] Nothing eURI Nothing Nothing []) `shouldBe` mAction

                 it "Link" $
                   encode (Link [] [] eURI Nothing Nothing) `shouldBe` mLink

                 it "SubEntity_EmbeddedRepresentation" $
                   encode (EmbeddedRepresentation (Entity [] Map.empty [] [] [] Nothing) []) `shouldBe` mSubEntity_EmbeddedRepresentation

                 it "SubEntity_EmbeddedLink" $
                   encode (EmbeddedLink (Link [] [] eURI Nothing Nothing)) `shouldBe` mSubEntity_EmbeddedLink

                 it "Entity" $
                   encode (Entity [] Map.empty [] [] [] Nothing) `shouldBe` mEntity

  where mEntity                           = "{}" :: BL.ByteString
        mSubEntity_EmbeddedLink           = "{\"href\":\"http://example.com\",\"rel\":[]}" :: BL.ByteString
        mSubEntity_EmbeddedRepresentation = "{\"rel\":[]}" :: BL.ByteString
        mLink                             = "{\"href\":\"http://example.com\",\"rel\":[]}" :: BL.ByteString
        mAction                           = "{\"href\":\"http://example.com\",\"name\":\"name\"}" :: BL.ByteString
        mField                            = "{\"name\":\"name\"}" :: BL.ByteString
        
        eURI = fromJust $ parseURIReference "http://example.com"
