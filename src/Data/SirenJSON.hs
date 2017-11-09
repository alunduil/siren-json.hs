{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Data.SirenJSON
Description : Types and Instances for @application/vnd.siren+json@
Copyright   : (c) Alex Brandt, 2017
License     : MIT

A collection of types and instances for @application/vnd.siren+json@.

Full documentation for @application/vnd.siren+json@ can be found at
<https://github.com/kevinswiber/siren>.
-}
module Data.SirenJSON where

import Data.Aeson ((.=), (.:?), (.!=), (.:), FromJSON (parseJSON), Object, object, ToJSON (toJSON), Value (Object, String), withObject, withText)
import Data.Foldable (asum)
import Data.Functor ((<$>))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (StdMethod (GET))
import Network.URI (URI)

import qualified Data.HashMap.Lazy as HashMap (fromList)
import qualified Data.Map.Strict as Map (empty, Map, null)

import External.Network.HTTP.Media.MediaType.JSON ()
import External.Network.HTTP.Types.Method.JSON ()
import External.Network.URI.JSON ()

-- * Core Data Types

-- | The top-level object for an @application/vnd.siren+json@ resource.
data Entity = Entity
  { eClass      :: [Text]            -- ^ Nature of 'Entity''s
                                     --   content---implementation dependant and
                                     --   should be documented
  , eProperties :: Map.Map Text Text -- ^ 'Entity' state as key-value pairs---we
                                     --   encode all values as strings for
                                     --   simplicity and to allow parsing into
                                     --   other types as needed
  , eEntities   :: [SubEntity]
  , eLinks      :: [Link]
  , eActions    :: [Action]
  , eTitle      :: Maybe Text        -- ^ Descriptive text about 'Entity'
  } deriving (Eq, Show)

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \ v -> do
    eClass      <- v .:? "class"      .!= []
    eProperties <- v .:? "properties" .!= Map.empty
    eEntities   <- v .:? "entities"   .!= []
    eLinks      <- v .:? "links"      .!= []
    eActions    <- v .:? "actions"    .!= []
    eTitle      <- v .:? "title"

    return Entity{..}

instance ToJSON Entity where
  toJSON Entity{..} = object $ catMaybes
    [ if null eClass          then Nothing else Just $ "class"      .= eClass
    , if Map.null eProperties then Nothing else Just $ "properties" .= eProperties
    , if null eEntities       then Nothing else Just $ "entities"   .= eEntities
    , if null eLinks          then Nothing else Just $ "links"      .= eLinks
    , if null eActions        then Nothing else Just $ "actions"    .= eActions
    , (.=) "title" <$> eTitle
    ]

-- | Nested object for an @application/vnd.siren+json@.
data SubEntity = EmbeddedLink Link
               | EmbeddedRepresentation
                   { sEntity :: Entity
                   , sRel   :: [Text]
                   }
  deriving (Eq, Show)

instance FromJSON SubEntity where
  parseJSON = withObject "SubEntity" $ \ v -> asum
    [ EmbeddedLink <$> parseJSON (Object v)
    , do sEntity <- parseJSON (Object v)
         sRel    <- v .: "rel"

         return EmbeddedRepresentation{..}
    ]

instance ToJSON SubEntity where
  toJSON (EmbeddedLink l) = toJSON l
  toJSON EmbeddedRepresentation{..} = Object $ toObject sEntity <> HashMap.fromList ["rel" .= sRel]
                                      where toObject :: ToJSON a => a -> Object
                                            toObject v = case toJSON v of
                                                           Object o -> o
                                                           _        -> error "toObject: received non-Object"

-- | Link to a related resource.
data Link = Link
  { lClass :: [Text]          -- ^ Nature of 'Entity''s content---
                              --   implementation dependant and should be
                              --   documented
  , lRel   :: [Text]
  , lHref  :: URI
  , lType  :: Maybe MediaType
  , lTitle :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \ v -> do
    lClass <- v .:? "class" .!= []
    lRel   <- v .:  "rel"
    lHref  <- v .:  "href"
    lType  <- v .:? "type"
    lTitle <- v .:? "title"

    return Link{..}

instance ToJSON Link where
  toJSON Link{..} = object $ catMaybes
    [ if null lClass then Nothing else Just $ "class" .= lClass
    , Just $ "rel"  .= lRel
    , Just $ "href" .= lHref
    , (.=) "type"  <$> lType
    , (.=) "title" <$> lTitle
    ]

-- | Behavior of an 'Entity'.
data Action = Action
  { aName   :: Text
  , aClass  :: [Text]
  , aMethod :: Maybe StdMethod
  , aHref   :: URI
  , aTitle  :: Maybe Text
  , aType   :: Maybe MediaType
  , aFields :: [Field]
  } deriving (Eq, Show)

instance FromJSON Action where
  parseJSON = withObject "Action" $ \ v -> do
    aName   <- v .:  "name"
    aClass  <- v .:? "class"  .!= []
    aMethod <- v .:? "method" .!= Just GET
    aHref   <- v .:  "href"
    aTitle  <- v .:? "title"
    aType   <- v .:? "type"
    aFields <- v .:? "fields" .!= []

    return Action{..}

instance ToJSON Action where
  toJSON Action{..} = object $ catMaybes
    [ Just $ "name" .= aName
    , if null aClass then Nothing else Just $ "class"  .= aClass
    , (.=) "method" <$> aMethod
    , Just $ "href" .= aHref
    , (.=) "title" <$> aTitle
    , (.=) "type"  <$> (show <$> aType)
    , if null aFields then Nothing else Just $ "fields" .= aFields
    ]

-- | Control inside of an 'Action'.
data Field = Field
  { fName  :: Text
  , fClass :: [Text]
  , fType  :: Maybe InputType
  , fValue :: Maybe Text
  , fTitle :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON Field where
  parseJSON = withObject "Field" $ \ v -> do
    fName  <- v .:  "name"
    fClass <- v .:? "class" .!= []
    fType  <- v .:? "type"  .!= Just Text
    fValue <- v .:? "value"
    fTitle <- v .:? "title"

    return Field{..}

instance ToJSON Field where
  toJSON Field{..} = object $ catMaybes
    [ Just $ "name" .= fName
    , if null fClass then Nothing else Just $ "class" .= fClass
    , (.=) "type"  <$> fType
    , (.=) "value" <$> fValue
    , (.=) "title" <$> fTitle
    ]

data InputType = Hidden
               | Text
               | Search
               | Tel
               | URL
               | Email
               | Password
               | DateTime
               | Date
               | Month
               | Week
               | Time
               | DateTimeLocal
               | Number
               | Range
               | Color
               | CheckBox
               | Radio
               | File
  deriving (Eq, Show)

instance FromJSON InputType where
  parseJSON = withText "InputType" $ \ v ->
    case v of
      "hidden"         -> return Hidden
      "text"           -> return Text
      "search"         -> return Search
      "tel"            -> return Tel
      "url"            -> return URL
      "email"          -> return Email
      "password"       -> return Password
      "datetime"       -> return DateTime
      "date"           -> return Date
      "month"          -> return Month
      "week"           -> return Week
      "time"           -> return Time
      "datetime-local" -> return DateTimeLocal
      "number"         -> return Number
      "range"          -> return Range
      "color"          -> return Color
      "checkbox"       -> return CheckBox
      "radio"          -> return Radio
      "file"           -> return File
      _                -> fail "invalid InputType"
    
instance ToJSON InputType where
  toJSON Hidden        = String "hidden"
  toJSON Text          = String "text"
  toJSON Search        = String "search"
  toJSON Tel           = String "tel"
  toJSON URL           = String "url"
  toJSON Email         = String "email"
  toJSON Password      = String "password"
  toJSON DateTime      = String "datetime"
  toJSON Date          = String "date"
  toJSON Month         = String "month"
  toJSON Week          = String "week"
  toJSON Time          = String "time"
  toJSON DateTimeLocal = String "datetime-local"
  toJSON Number        = String "number"
  toJSON Range         = String "range"
  toJSON Color         = String "color"
  toJSON CheckBox      = String "checkbox"
  toJSON Radio         = String "radio"
  toJSON File          = String "file"

-- * Type Conversion

-- | A type that can be converted from 'Entity'.
class FromEntity a where
  fromEntity :: Entity -> a

-- | A type that can be converted to 'Entity'.
class ToEntity a where
  toEntity :: a -> Entity
