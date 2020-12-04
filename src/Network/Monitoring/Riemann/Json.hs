{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Monitoring.Riemann.Json where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON,
    ToJSON,
    parseJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Scientific (toBoundedInteger, toBoundedRealFloat)
import Network.Monitoring.Riemann.Proto.Attribute (Attribute)
import Network.Monitoring.Riemann.Proto.Event (Event (..))
import Network.Monitoring.Riemann.Proto.Msg (Msg (..))
import Network.Monitoring.Riemann.Proto.Query (Query (..))
import Network.Monitoring.Riemann.Proto.State (State (..))
import Prelude hiding (error)

instance ToJSON Attribute

instance FromJSON Attribute

instance ToJSON Event

instance FromJSON Event where
  parseJSON =
    withObject "Event" $ \v -> do
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
      let metric_sint64 = mMetric_sint64 <|> (toBoundedInteger =<< mMetric)
          metric_d =
            mMetric_d <|> (rightToJust . toBoundedRealFloat =<< mMetric)
          metric_f =
            mMetric_f <|> (rightToJust . toBoundedRealFloat =<< mMetric)
      pure Event {..}

instance ToJSON Query

instance FromJSON Query where
  parseJSON =
    withObject "Query" $ \v -> do
      string <- v .: "string"
      pure Query {..}

instance ToJSON State

instance FromJSON State where
  parseJSON =
    withObject "State" $ \v -> do
      time <- v .:? "time"
      state <- v .:? "state"
      service <- v .:? "service"
      host <- v .:? "host"
      description <- v .:? "description"
      once <- v .:? "once"
      tags <- v .:? "tags" .!= []
      ttl <- v .:? "ttl"
      pure State {..}

instance ToJSON Msg

instance FromJSON Msg where
  parseJSON =
    withObject "Msg" $ \v -> do
      ok <- v .:? "ok"
      error <- v .:? "error"
      states <- v .: "states" .!= []
      query <- v .:? "query"
      events <- v .: "events" .!= []
      pure Msg {..}

rightToJust :: Either l r -> Maybe r
rightToJust (Left _) = Nothing
rightToJust (Right v) = Just v
