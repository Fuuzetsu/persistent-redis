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
import Database.Persist.Redis.Config (RedisT(..))
import Database.Persist.Redis.Internal

data RedisBackend

toOid :: (PersistEntity val) => Integer -> Key val
toOid = Key . PersistText . pack . show 

runDB :: (R.RedisCtx m f) => m (f a) -> RedisT m (f a)
runDB x = undefined

insertImpl :: (R.RedisCtx m f, PersistEntity val) => val -> m (f Integer)
insertImpl val = do
    let fields = toInsertFields val
    let keyId  = toKeyId val
    oid <- R.incr keyId
    let key    = toKey val
    status <- R.hmset key fields
    return oid

getKey :: R.RedisCtx m f => f Integer -> Integer
getKey = undefined

instance (Applicative m, Functor m, MonadIO m, MonadBaseControl IO m) => PersistStore (RedisT m) where
    type PersistMonadBackend (RedisT m) = RedisBackend

    insert val = do
        liftIO $ print "insert"
        fail "insert"

    insertKey k record = undefined

    repsert k record = undefined

    replace k record = undefined

    delete k = undefined

    get k = undefined