{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric,
  FlexibleInstances, MultiParamTypeClasses #-}

module Network.Monitoring.Riemann.Proto.Event
  ( Event(..)
  ) where

import qualified Data.Data as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Network.Monitoring.Riemann.Proto.Attribute as Proto (Attribute)
import Prelude ((+))
import qualified Prelude as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Event = Event
  { time :: !(P'.Maybe P'.Int64)
  , state :: !(P'.Maybe P'.Utf8)
  , service :: !(P'.Maybe P'.Utf8)
  , host :: !(P'.Maybe P'.Utf8)
  , description :: !(P'.Maybe P'.Utf8)
  , tags :: !(P'.Seq P'.Utf8)
  , ttl :: !(P'.Maybe P'.Float)
  , attributes :: !(P'.Seq Proto.Attribute)
  , metric_sint64 :: !(P'.Maybe P'.Int64)
  , metric_d :: !(P'.Maybe P'.Double)
  , metric_f :: !(P'.Maybe P'.Float)
  } deriving ( Prelude'.Show
             , Prelude'.Eq
             , Prelude'.Ord
             , Prelude'.Typeable
             , Prelude'.Data
             , Prelude'.Generic
             )

instance P'.Mergeable Event where
  mergeAppend (Event x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11) (Event y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11) =
    Event
      (P'.mergeAppend x'1 y'1)
      (P'.mergeAppend x'2 y'2)
      (P'.mergeAppend x'3 y'3)
      (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)

instance P'.Default Event where
  defaultValue =
    Event
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire Event where
  wireSize ft' self'@(Event x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11) =
    case ft' of
      10 -> calc'Size
      11 -> P'.prependMessageSize calc'Size
      _ -> P'.wireSizeErr ft' self'
    where
      calc'Size =
        P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3 +
        P'.wireSizeOpt 1 9 x'4 +
        P'.wireSizeOpt 1 9 x'5 +
        P'.wireSizeRep 1 9 x'6 +
        P'.wireSizeOpt 1 2 x'7 +
        P'.wireSizeRep 1 11 x'8 +
        P'.wireSizeOpt 1 18 x'9 +
        P'.wireSizeOpt 1 1 x'10 +
        P'.wireSizeOpt 1 2 x'11
  wirePut ft' self'@(Event x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11) =
    case ft' of
      10 -> put'Fields
      11 -> do
        P'.putSize (P'.wireSize 10 self')
        put'Fields
      _ -> P'.wirePutErr ft' self'
    where
      put'Fields = do
        P'.wirePutOpt 8 3 x'1
        P'.wirePutOpt 18 9 x'2
        P'.wirePutOpt 26 9 x'3
        P'.wirePutOpt 34 9 x'4
        P'.wirePutOpt 42 9 x'5
        P'.wirePutRep 58 9 x'6
        P'.wirePutOpt 69 2 x'7
        P'.wirePutRep 74 11 x'8
        P'.wirePutOpt 104 18 x'9
        P'.wirePutOpt 113 1 x'10
        P'.wirePutOpt 125 2 x'11
  wireGet ft' =
    case ft' of
      10 -> P'.getBareMessageWith update'Self
      11 -> P'.getMessageWith update'Self
      _ -> P'.wireGetErr ft'
    where
      update'Self wire'Tag old'Self =
        case wire'Tag of
          8 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {time = Prelude'.Just new'Field})
              (P'.wireGet 3)
          18 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {state = Prelude'.Just new'Field})
              (P'.wireGet 9)
          26 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {service = Prelude'.Just new'Field})
              (P'.wireGet 9)
          34 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {host = Prelude'.Just new'Field})
              (P'.wireGet 9)
          42 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {description = Prelude'.Just new'Field})
              (P'.wireGet 9)
          58 ->
            Prelude'.fmap
              (\ !new'Field ->
                 old'Self {tags = P'.append (tags old'Self) new'Field})
              (P'.wireGet 9)
          69 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {ttl = Prelude'.Just new'Field})
              (P'.wireGet 2)
          74 ->
            Prelude'.fmap
              (\ !new'Field ->
                 old'Self
                   {attributes = P'.append (attributes old'Self) new'Field})
              (P'.wireGet 11)
          104 ->
            Prelude'.fmap
              (\ !new'Field ->
                 old'Self {metric_sint64 = Prelude'.Just new'Field})
              (P'.wireGet 18)
          113 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {metric_d = Prelude'.Just new'Field})
              (P'.wireGet 1)
          125 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {metric_f = Prelude'.Just new'Field})
              (P'.wireGet 2)
          _ ->
            let (field'Number, wire'Type) = P'.splitWireTag wire'Tag
             in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Event) Event where
  getVal m' f' = f' m'

instance P'.GPB Event

instance P'.ReflectDescriptor Event where
  getMessageInfo _ =
    P'.GetMessageInfo
      (P'.fromDistinctAscList [])
      (P'.fromDistinctAscList [8, 18, 26, 34, 42, 58, 69, 74, 104, 113, 125])
  reflectDescriptorInfo _ =
    Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Proto.Event\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"Event\"}, descFilePath = [\"Network\",\"Monitoring\",\"Riemann\",\"Proto\",\"Event.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.time\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"time\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.state\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"state\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.service\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"service\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.host\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"host\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.description\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"description\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.tags\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"tags\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.ttl\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"ttl\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 69}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.attributes\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"attributes\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Proto.Attribute\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"Attribute\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.metric_sint64\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"metric_sint64\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 104}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.metric_d\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"metric_d\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 113}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Event.metric_f\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Event\"], baseName' = FName \"metric_f\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 125}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Event where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Event where
  textPut msg = do
    P'.tellT "time" (time msg)
    P'.tellT "state" (state msg)
    P'.tellT "service" (service msg)
    P'.tellT "host" (host msg)
    P'.tellT "description" (description msg)
    P'.tellT "tags" (tags msg)
    P'.tellT "ttl" (ttl msg)
    P'.tellT "attributes" (attributes msg)
    P'.tellT "metric_sint64" (metric_sint64 msg)
    P'.tellT "metric_d" (metric_d msg)
    P'.tellT "metric_f" (metric_f msg)
  textGet = do
    mods <-
      P'.sepEndBy
        (P'.choice
           [ parse'time
           , parse'state
           , parse'service
           , parse'host
           , parse'description
           , parse'tags
           , parse'ttl
           , parse'attributes
           , parse'metric_sint64
           , parse'metric_d
           , parse'metric_f
           ])
        P'.spaces
    Prelude'.return (Prelude'.foldl (\v f -> f v) P'.defaultValue mods)
    where
      parse'time =
        P'.try
          (do v <- P'.getT "time"
              Prelude'.return (\o -> o {time = v}))
      parse'state =
        P'.try
          (do v <- P'.getT "state"
              Prelude'.return (\o -> o {state = v}))
      parse'service =
        P'.try
          (do v <- P'.getT "service"
              Prelude'.return (\o -> o {service = v}))
      parse'host =
        P'.try
          (do v <- P'.getT "host"
              Prelude'.return (\o -> o {host = v}))
      parse'description =
        P'.try
          (do v <- P'.getT "description"
              Prelude'.return (\o -> o {description = v}))
      parse'tags =
        P'.try
          (do v <- P'.getT "tags"
              Prelude'.return (\o -> o {tags = P'.append (tags o) v}))
      parse'ttl =
        P'.try
          (do v <- P'.getT "ttl"
              Prelude'.return (\o -> o {ttl = v}))
      parse'attributes =
        P'.try
          (do v <- P'.getT "attributes"
              Prelude'.return
                (\o -> o {attributes = P'.append (attributes o) v}))
      parse'metric_sint64 =
        P'.try
          (do v <- P'.getT "metric_sint64"
              Prelude'.return (\o -> o {metric_sint64 = v}))
      parse'metric_d =
        P'.try
          (do v <- P'.getT "metric_d"
              Prelude'.return (\o -> o {metric_d = v}))
      parse'metric_f =
        P'.try
          (do v <- P'.getT "metric_f"
              Prelude'.return (\o -> o {metric_f = v}))
