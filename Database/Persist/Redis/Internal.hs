{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Redis.Internal
	( toInsertFields
    , toKey
    , toKeyId
    , toEntityName
    , toKeyText
    , toB
    , mkEntity
	) where

import Data.Data
import Data.Text (Text, unpack)
import Database.Persist.Types
import Database.Persist.Class
import Data.Aeson.Generic (encode)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.UTF8 as U

toLabel :: FieldDef a -> B.ByteString
toLabel = U.fromString . unpack . unDBName . fieldDB

toEntityName :: EntityDef a -> B.ByteString
toEntityName = U.fromString . unpack . unDBName . entityDB

moveToByteString :: Data a => Either Text a -> B.ByteString
moveToByteString (Left a)  = U.fromString $ unpack a
moveToByteString (Right a) = toStrict $ encode a

toValue :: PersistValue -> B.ByteString
toValue (PersistText x) = U.fromString $ unpack x
toValue (PersistByteString x) = x
toValue (PersistInt64 x) = U.fromString $ show x
toValue (PersistDouble x) = U.fromString $ show x
toValue (PersistBool x) = U.fromString $ show x
toValue (PersistDay x) = U.fromString $ show x
toValue (PersistTimeOfDay x) = U.fromString $ show x
toValue (PersistUTCTime x) = U.fromString $ show x
toValue (PersistNull) = U.fromString ""
toValue (PersistList x) = U.fromString $ show x
toValue (PersistMap x) = U.fromString $ show x
toValue (PersistRational _) = undefined
toValue (PersistZonedTime _) = undefined
toValue (PersistObjectId _) = error "PersistObjectId is not supported."

mkEntity :: (Monad m, PersistEntity val) => EntityDef a -> [(B.ByteString, B.ByteString)] -> m (Entity val)
mkEntity = undefined

zipAndConvert :: PersistField t => [FieldDef a] -> [t] -> [(B.ByteString, B.ByteString)]
zipAndConvert [] _ = []
zipAndConvert _ [] = []
zipAndConvert (e:efields) (p:pfields) = 
    let pv = toPersistValue p
    in
        if pv == PersistNull then zipAndConvert efields pfields
            else (toLabel e, toValue pv) : zipAndConvert efields pfields

-- | Create a list for create/update in Redis store
toInsertFields :: PersistEntity val => val -> [(B.ByteString, B.ByteString)]
toInsertFields record = zipAndConvert entity fields
    where
        entity = entityFields $ entityDef $ Just record
        fields = toPersistFields record

underscoreBs :: B.ByteString
underscoreBs = U.fromString "_"

-- | Make a key for given entity and id
toKey :: PersistEntity val => val -> Integer -> B.ByteString
toKey val n = B.append (toObjectPrefix val) (U.fromString $ show n)

toKeyText :: PersistEntity val => val -> Text -> B.ByteString
toKeyText val k = B.append (toObjectPrefix val) (U.fromString $ unpack k)

toB :: Text -> B.ByteString
toB = U.fromString . unpack

-- | Create a string key for given entity
toObjectPrefix :: PersistEntity val => val -> B.ByteString
toObjectPrefix val = B.append (toEntityName $ entityDef $ Just val) underscoreBs

idBs :: B.ByteString
idBs = U.fromString "id"

-- | Construct an id key, that is incremented for access
toKeyId :: PersistEntity val => val -> B.ByteString
toKeyId val = B.append (toObjectPrefix val) idBs