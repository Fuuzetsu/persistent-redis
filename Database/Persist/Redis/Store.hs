{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Store 
    ( RedisBackend
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

runDB :: (R.RedisCtx m f) => m (f a) -> RedisT m (f a)
runDB x = undefined

createKey :: (R.RedisCtx m f, PersistEntity val) => val -> m (f Integer)
createKey val = do
    let keyId = toKeyId val
    oid <- R.incr keyId
    return oid

insertImpl :: (R.RedisCtx m f, PersistEntity val) => val -> Integer -> m (f R.Status)
insertImpl val keyId = do
    let fields = toInsertFields val
    let key    = toKey val keyId
    R.hmset key fields

desugar :: R.TxResult a -> Either String a
desugar (R.TxSuccess x) =  Right x
desugar R.TxAborted = Left "Transaction aborted!"
desugar (R.TxError string) = Left string

execRedisT :: (Monad m, MonadIO m) => R.RedisTx (R.Queued a) -> RedisT m a
execRedisT action = do
    conn <- thisConnection
    result <- liftIO $ R.runRedis conn $ R.multiExec action
    let r = desugar result
    case r of
        (Right x) -> return x
        (Left x)  -> fail x


getKey :: R.RedisCtx m f => f Integer -> Integer
getKey = undefined

instance (Applicative m, Functor m, MonadIO m, MonadBaseControl IO m) => PersistStore (RedisT m) where
    type PersistMonadBackend (RedisT m) = RedisBackend

    insert val = do
        key <- execRedisT $ createKey val
        r <- execRedisT $ insertImpl val key
        liftIO $ print r
        return $ toOid key

    insertKey k record = undefined

    repsert k record = undefined

    replace k record = undefined

    delete k = undefined

    get k = undefined