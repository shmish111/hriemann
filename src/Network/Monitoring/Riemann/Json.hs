{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC  -fno-warn-orphans #-}
module Network.Monitoring.Riemann.Json where

import           Data.Aeson                                 (FromJSON, ToJSON,
                                                             Value (String),
                                                             parseJSON, toJSON)
import           Data.Aeson.Types                           (typeMismatch)
import qualified Data.Text                                  as Text
import           Network.Monitoring.Riemann.Proto.Attribute as PA
import           Network.Monitoring.Riemann.Proto.Event     as PE
import qualified Text.ProtocolBuffers.Header                as P'

instance ToJSON P'.Utf8 where
  toJSON v = String (Text.pack (P'.uToString v))

instance FromJSON P'.Utf8 where
  parseJSON (String s) = pure. P'.uFromString . Text.unpack $ s
  parseJSON invalid    = typeMismatch "non-Utf8 String" invalid

instance ToJSON PA.Attribute

instance FromJSON PA.Attribute

instance ToJSON PE.Event

instance FromJSON PE.Event
