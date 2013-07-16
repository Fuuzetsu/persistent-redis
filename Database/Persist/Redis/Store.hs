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
import Data.Text (pack)
import Database.Persist.Redis.Config (RedisT(..), thisConnection)
import Database.Persist.Redis.Internal

data RedisBackend

toOid :: (PersistEntity val) => Integer -> Key val
toOid = Key . PersistText . pack . show 

-- | Fetches a next key from <object>_id record
createKey :: (R.RedisCtx m f, PersistEntity val) => val -> m (f Integer)
createKey val = do
    let keyId = toKeyId val
    oid <- R.incr keyId
    return oid

-- | Inserts a hash map into <object>_<id> record
insertImpl :: (R.RedisCtx m f, PersistEntity val) => val -> Integer -> m (f R.Status)
insertImpl val keyId = do
    let fields = toInsertFields val
    let key    = toKey val keyId
    R.hmset key fields

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
        key <- execRedisT $ createKey val
        r <- execRedisT $ insertImpl val key
        return $ toOid key

    insertKey (Key (PersistText key)) val = do
        let fields = toInsertFields val
        let redisKey = toKeyText val key
        r <- execRedisT $ R.hmset redisKey fields
        return ()

    repsert k record = undefined

    replace k record = undefined

    delete k = undefined

    get k = undefined