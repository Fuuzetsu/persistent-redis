{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Redis.Internal
	( toInsertFields
    , toKey
    , toKeyId
    , toEntityName
	) where

import Data.Text (unpack)
import Database.Persist.Types
import Database.Persist.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U

toLabel :: FieldDef a -> B.ByteString
toLabel = U.fromString . unpack . unDBName . fieldDB

toEntityName :: EntityDef a -> B.ByteString
toEntityName = U.fromString . unpack . unDBName . entityDB

toValue :: PersistField a => a -> B.ByteString
toValue = undefined

zipAndConvert :: PersistField t => [FieldDef a] -> [t] -> [(B.ByteString, B.ByteString)]
zipAndConvert [] _ = []
zipAndConvert _ [] = []
zipAndConvert (e:efields) (p:pfields) = 
    let pv = toPersistValue p
    in
        if pv == PersistNull then zipAndConvert efields pfields
            else (toLabel e, toValue pv) : zipAndConvert efields pfields

-- Create a list for create/update in Redis store
toInsertFields :: PersistEntity val => val -> [(B.ByteString, B.ByteString)]
toInsertFields record = zipAndConvert entity fields
    where
        entity = entityFields $ entityDef $ Just record
        fields = toPersistFields record

underscoreBs :: B.ByteString
underscoreBs = U.fromString "_"

-- Create a string key for given entity
toKey :: PersistEntity val => val -> B.ByteString
toKey val = B.append (toEntityName $ entityDef $ Just val) underscoreBs

idBs :: B.ByteString
idBs = U.fromString "_id"

-- Construct an id key, that is incremented for access
toKeyId :: PersistEntity val => val -> B.ByteString
toKeyId val = B.append (toKey val) idBs