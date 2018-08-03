{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric,
  FlexibleInstances, MultiParamTypeClasses #-}

module Network.Monitoring.Riemann.Proto.Query
  ( Query(..)
  ) where

import qualified Data.Data as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Prelude as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

newtype Query = Query
  { string :: P'.Maybe P'.Utf8
  } deriving ( Prelude'.Show
             , Prelude'.Eq
             , Prelude'.Ord
             , Prelude'.Typeable
             , Prelude'.Data
             , Prelude'.Generic
             )

instance P'.Mergeable Query where
  mergeAppend (Query x'1) (Query y'1) = Query (P'.mergeAppend x'1 y'1)

instance P'.Default Query where
  defaultValue = Query P'.defaultValue

instance P'.Wire Query where
  wireSize ft' self'@(Query x'1) =
    case ft' of
      10 -> calc'Size
      11 -> P'.prependMessageSize calc'Size
      _ -> P'.wireSizeErr ft' self'
    where
      calc'Size = P'.wireSizeOpt 1 9 x'1
  wirePut ft' self'@(Query x'1) =
    case ft' of
      10 -> put'Fields
      11 -> do
        P'.putSize (P'.wireSize 10 self')
        put'Fields
      _ -> P'.wirePutErr ft' self'
    where
      put'Fields = P'.wirePutOpt 10 9 x'1
  wireGet ft' =
    case ft' of
      10 -> P'.getBareMessageWith update'Self
      11 -> P'.getMessageWith update'Self
      _ -> P'.wireGetErr ft'
    where
      update'Self wire'Tag old'Self =
        case wire'Tag of
          10 ->
            Prelude'.fmap
              (\ !new'Field -> old'Self {string = Prelude'.Just new'Field})
              (P'.wireGet 9)
          _ ->
            let (field'Number, wire'Type) = P'.splitWireTag wire'Tag
             in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Query) Query where
  getVal m' f' = f' m'

instance P'.GPB Query

instance P'.ReflectDescriptor Query where
  getMessageInfo _ =
    P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _ =
    Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Proto.Query\", haskellPrefix = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule = [MName \"Proto\"], baseName = MName \"Query\"}, descFilePath = [\"Network\",\"Monitoring\",\"Riemann\",\"Proto\",\"Query.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Proto.Query.string\", haskellPrefix' = [MName \"Network\",MName \"Monitoring\",MName \"Riemann\"], parentModule' = [MName \"Proto\",MName \"Query\"], baseName' = FName \"string\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Query where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Query where
  textPut msg = P'.tellT "string" (string msg)
  textGet = do
    mods <- P'.sepEndBy (P'.choice [parse'string]) P'.spaces
    Prelude'.return (Prelude'.foldl (\v f -> f v) P'.defaultValue mods)
    where
      parse'string =
        P'.try
          (do v <- P'.getT "string"
              Prelude'.return (\o -> o {string = v}))
