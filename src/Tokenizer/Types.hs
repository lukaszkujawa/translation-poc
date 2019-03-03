{-# LANGUAGE DeriveGeneric #-}

module Tokenizer.Types where

import GHC.Generics
import Data.Text (Text)

data Document = Document { session :: Session } deriving (Generic, Show)

data Session = Session { events :: [Event] } deriving (Generic, Show)

data Event =
    EventPageLoad { etype :: Text , metadatapageload :: Maybe MetadataPageLoad }
  | EventClick { etype :: Text , metadataclick :: Maybe MetadataClick }
  | EventLogin { etype :: Text , metadatalogin :: Maybe MetadataLogin }
  | EventPlayVideo { etype :: Text , metadataplayvideo :: Maybe MetadataPlayVideo }
  | EventUndefined
    deriving (Generic, Show)

data MetadataPageLoad = MetadataPageLoad { url :: Text } deriving (Generic, Show)
data MetadataClick = MetadataClick { target :: Text } deriving (Generic, Show)
data MetadataLogin = MetadataLogin { username :: Text, package :: Text } deriving (Generic, Show)
data MetadataPlayVideo = MetadataPlayVideo { id :: Text, genre :: Text } deriving (Generic, Show)
