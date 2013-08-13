{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Store 
    ( RedisBackend
    , execRedisT
    )where

import Database.Persist
import Control.Applicative (Applicative, (<$>))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader.Class 
import Control.Monad.Reader(ReaderT(..), runReaderT)
import qualified Database.Redis as R
import Data.Text (Text, pack)
import Database.Persist.Redis.Config (RedisT(..), thisConnection)
import Database.Persist.Redis.Internal

data RedisBackend

dummyFromKey :: KeyBackend RedisBackend v -> v
dummyFromKey _ = error "dummyFromKey"

toOid :: (PersistEntity val) => Text -> Key val
toOid = Key . PersistText

-- | Fetches a next key from <object>_id record
createKey :: (R.RedisCtx m f, PersistEntity val) => val -> m (f Integer)
createKey val = do
    let keyId = toKeyId val
    oid <- R.incr keyId
    return oid

desugar :: R.TxResult a -> Either String a
desugar (R.TxSuccess x) =  Right x
desugar R.TxAborted = Left "Transaction aborted!"
desugar (R.TxError string) = Left string

-- | Execute Redis transaction inside RedisT monad transformer
execRedisT :: (Monad m, MonadIO m) => R.RedisTx (R.Queued a) -> RedisT m a
execRedisT action = do
    conn <- thisConnection
    result <- liftIO $ R.runRedis conn $ R.multiExec action -- this is the question if we should support transaction here
    let r = desugar result
    case r of
        (Right x) -> return x
        (Left x)  -> fail x

instance (Applicative m, Functor m, MonadIO m, MonadBaseControl IO m) => PersistStore (RedisT m) where
    type PersistMonadBackend (RedisT m) = RedisBackend

    insert val = do
        keyId <- execRedisT $ createKey val
        let key    = toOid $ toKeyText val keyId
        r <- insertKey key val
        return $ key

    insertKey (Key (PersistText key)) val = do
        let fields = toInsertFields val
        -- Inserts a hash map into <object>_<id> record
        r <- execRedisT $ R.hmset (toB key) fields
        return ()

    repsert k record = undefined

    replace k record = undefined

    delete (Key (PersistText key)) = do
        r <- execRedisT $ R.del [toB key]
        case r of
            0 -> fail "there is no such key!"
            1 -> return ()
            otherwise -> fail "there are a lot of such keys!"

    get k@(Key (PersistText key)) = do
        let t = entityDef $ Just $ dummyFromKey k
        r <- execRedisT $ R.hgetall (toB key)
        if (length r) == 0
            then return Nothing
            else do
                 return Nothing
            
