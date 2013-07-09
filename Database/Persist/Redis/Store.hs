{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Redis.Store 
    ( RedisBackend
    )where

import Database.Persist
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Database.Redis as R

import Database.Persist.Redis.Config (RedisT)
import Database.Persist.Redis.Internal

data RedisBackend

instance (Applicative m, Functor m, MonadIO m, MonadBaseControl IO m) => PersistStore (RedisT m) where
    type PersistMonadBackend (RedisT m) = RedisBackend

    insert val = undefined

    insertKey k record = undefined

    repsert k record = undefined

    replace k record = undefined

    delete k = undefined

    get k = undefined