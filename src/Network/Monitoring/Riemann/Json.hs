{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC  -fno-warn-orphans #-}
module Network.Monitoring.Riemann.Json where

import           Data.Aeson                                 (FromJSON, ToJSON,
                                                             Value (String),
                                                             parseJSON, toJSON,
                                                             withObject,
                                                             withText, (.!=),
                                                             (.:?))
import           Data.Scientific                            (toBoundedInteger,
                                                             toBoundedRealFloat)
import qualified Data.Text                                  as Text
import           Network.Monitoring.Riemann.Proto.Attribute as PA
import           Network.Monitoring.Riemann.Proto.Event     as PE
import qualified Text.ProtocolBuffers.Header                as P'

instance ToJSON P'.Utf8 where
  toJSON v = String (Text.pack (P'.uToString v))

instance FromJSON P'.Utf8 where
  parseJSON = withText "Utf8 String" $ pure . P'.uFromString . Text.unpack

instance ToJSON PA.Attribute

instance FromJSON PA.Attribute

instance ToJSON PE.Event

instance FromJSON PE.Event where
  parseJSON = withObject "Event" $ \v -> do
    time <- v .:? "time"
    state <- v .:? "state"
    service <- v .:? "service"
    host <- v .:? "host"
    description <- v .:? "description"
    tags <- v .:? "tags" .!= []
    ttl <- v .:? "ttl"
    attributes <- v .:? "attributes" .!= []
    mMetric_sint64 <- v .:? "metric_sint64"
    mMetric_d <- v .:? "metric_d"
    mMetric_f <- v .:? "metric_f"
    mMetric <- v .:? "metric"
    let metric_sint64 = case mMetric_sint64 of
          Nothing -> do
            s <- mMetric
            toBoundedInteger s
          i -> i
    let metric_d = case mMetric_d of
          Nothing -> do
            s <- mMetric
            rightToJust $ toBoundedRealFloat s
          d -> d
    let metric_f = case mMetric_f of
          Nothing -> do
            s <- mMetric
            rightToJust $ toBoundedRealFloat s
          d -> d
    pure Event { .. }

rightToJust :: Either l r -> Maybe r
rightToJust (Left _)  = Nothing
rightToJust (Right v) = Just v
