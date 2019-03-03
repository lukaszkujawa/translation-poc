{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tokenizer
    ( from_file
    ) where

import Data.Aeson.Types
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString, readFile)
import Tokenizer.Types

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v -> Document
      <$> v .: "session"

instance FromJSON Session where
    parseJSON = withObject "Session" $ \v -> Session
        <$> v .: "events"

instance FromJSON Event where
    parseJSON = withObject "Event" $ \v -> do
        et <- v .: "etype"
        case et of
            String "page-load" -> params EventPageLoad v
            String "click" -> params EventClick v
            String "login" -> params EventLogin v
            String "play-video" -> params EventPlayVideo v
            _ -> return EventUndefined
        where params e v = e <$> v .: "etype" <*> v .:? "metadata"

instance FromJSON MetadataPageLoad

instance FromJSON MetadataClick

instance FromJSON MetadataLogin

instance FromJSON MetadataPlayVideo

session_from_json :: ByteString -> Maybe Document
session_from_json json = decode json

from_file :: String -> IO ([Event])
from_file path = do
    json <- Data.ByteString.Lazy.readFile path
    case session_from_json json of
      Just (Document d) -> return $ events d
      _ -> return []
