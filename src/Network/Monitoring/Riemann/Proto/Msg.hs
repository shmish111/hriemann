{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric,
  FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}

module Network.Monitoring.Riemann.Proto.Msg
  ( Msg(..)
  ) where

import qualified Data.Data as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Network.Monitoring.Riemann.Proto.Event as Proto (Event)
import qualified Network.Monitoring.Riemann.Proto.Query as Proto (Query)
import qualified Network.Monitoring.Riemann.Proto.State as Proto (State)
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Msg = Msg
  { ok :: !(P'.Maybe P'.Bool)
  , error :: !(P'.Maybe P'.Utf8)
  , states :: !(P'.Seq Proto.State)
  , query :: !(P'.Maybe Proto.Query)
  , events :: !(P'.Seq Proto.Event)
  } deriving ( Prelude'.Show
             , Prelude'.Eq
             , Prelude'.Ord
             , Prelude'.Typeable
             , Prelude'.Data
             , Prelude'.Generic
             )

instance P'.Mergeable Msg where
  mergeAppend (Msg x'1 x'2 x'3 x'4 x'5) (Msg y'1 y'2 y'3 y'4 y'5) =
    Msg
      (P'.mergeAppend x'1 y'1)
      (P'.mergeAppend x'2 y'2)
      (P'.mergeAppend x'3 y'3)
      (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default Msg where
  defaultValue =
    Msg
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire Msg where
  wireSize ft' self'@(Msg x'1 x'2 x'3 x'4 x'5) =
    case ft' of
      10 -> calc'Size
      11 -> P'.prependMessageSize calc'Size
      _ -> P'.wireSizeErr ft' self'
    where
      calc'Size =
        P'.wireSizeOpt 1 8 x'1 + P'.wireSizeOpt 1 9 x'2 +
        P'.wireSizeRep 1 11 x'3 +
        P'.wireSizeOpt 1 11 x'4 +
        P'.wireSizeRep 1 11 x'5
  wirePut ft' self'@(Msg x'1 x'2 x'3 x'4 x'5) =
    case ft' of
      10 -> put'Fields
      11 -> do
        P'.putSize (P'.wireSize 10 self')
        put'Fields
      _ -> P'.wirePutErr ft' self'
    where
      put'Fields = do
        P'.wirePutOpt 16 8 x'1
        P'.wirePutOpt 26 9 x'2
        P'.wirePutRep 34 11 x'3
        P'.wirePutOpt 42 11 x'4
        P'.wirePutRep 50 11 x'5
  wireGet ft' =
    case ft' of
      10 -> P'.getBareMessageWith update'Self
      11 -> P'.getMessageWith update'Self
      _ -> P'.wireGetErr ft'
    where
      update'Self wire'Tag old'Self =
        case wire'Tag of
          16 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {ok = Prelude'.Just new'Field})
              (P'.wireGet 8)
          26 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {error = Prelude'.Just new'Field})
              (P'.wireGet 9)
          34 ->
            Prelude'.fmap
              (\ !new'Field ->
                 old'Self {states = P'.append (states old'Self) new'Field})
              (P'.wireGet 11)
          42 ->
            Prelude'.fmap
              (\ !new'Field ->
                 old'Self
                   { query =
                       P'.mergeAppend (query old'Self) (Prelude'.Just new'Field)
                   })
              (P'.wireGet 11)
          50 ->
            Prelude'.fmap
              (\ !new'Field ->
                 old'Self {events = P'.append (events old'Self) new'Field})
              (P'.wireGet 11)
          _ ->
            let (field'Number, wire'Type) = P'.splitWireTag wire'Tag
             in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Msg) Msg where
  getVal m' f' = f' m'

instance P'.GPB Msg

instance P'.ReflectDescriptor Msg where
  getMessageInfo _ =
    P'.GetMessageInfo
      (P'.fromDistinctAscList [])
      (P'.fromDistinctAscList [16, 26, 34, 42, 50])
  reflectDescriptorInfo _ =
    Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Proto.Msg\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"Msg\"}, descFilePath = [\"Network\",\"Monitoring\",\"Riemann\",\"Proto\",\"Msg.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Msg.ok\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Msg\"], baseName' = FName \"ok\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Msg.error\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Msg\"], baseName' = FName \"error\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Msg.states\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Msg\"], baseName' = FName \"states\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Proto.State\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"State\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Msg.query\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Msg\"], baseName' = FName \"query\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Proto.Query\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"Query\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Msg.events\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Msg\"], baseName' = FName \"events\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Proto.Event\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"Event\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Msg where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Msg where
  textPut msg = do
    P'.tellT "ok" (ok msg)
    P'.tellT "error" (error msg)
    P'.tellT "states" (states msg)
    P'.tellT "query" (query msg)
    P'.tellT "events" (events msg)
  textGet = do
    mods <-
      P'.sepEndBy
        (P'.choice
           [parse'ok, parse'error, parse'states, parse'query, parse'events])
        P'.spaces
    Prelude'.return (Prelude'.foldl (\v f -> f v) P'.defaultValue mods)
    where
      parse'ok =
        P'.try
          (do v <- P'.getT "ok"
              Prelude'.return (\o -> o {ok = v}))
      parse'error =
        P'.try
          (do v <- P'.getT "error"
              Prelude'.return (\o -> o {error = v}))
      parse'states =
        P'.try
          (do v <- P'.getT "states"
              Prelude'.return (\o -> o {states = P'.append (states o) v}))
      parse'query =
        P'.try
          (do v <- P'.getT "query"
              Prelude'.return (\o -> o {query = v}))
      parse'events =
        P'.try
          (do v <- P'.getT "events"
              Prelude'.return (\o -> o {events = P'.append (events o) v}))
